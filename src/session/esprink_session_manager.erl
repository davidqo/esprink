%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%%
%%% When sessions_supervisor crashes session_manager also crashes.
%%% When particular session_supervisor crashes we only remove this
%%% hopeless session
%%%
%%% esprink_app_sup
%%%  |
%%% session_manager ---link---- sessions_supervisor
%%%  |                          |                |
%%%  --- monitor ---- session_supervisor1 ... session_supervisorN
%%%
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_session_manager).

-include("esprink.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_session/2,
    get_session_list/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    sessions_sup :: pid(),
    %% We need to make search both by session id and session_sup_ref.
    %% List will be preferable due small number of sessions
    sessions = [] :: list()
}).

-record(session, {
    id :: binary(),
    %% This is the ref of supervisor controlling this session.
    %% When session dies its supervisor restart it few times.
    %% When session supervisor dies we must remove this session
    %% as hopeless
    session_sup_ref :: reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

-record(add_session, {
    session_id :: binary(),
    options :: #{}
}).

-record(get_session_list, {
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_session(SessionId :: binary(), Options :: map()) -> ok | already_added.
add_session(SessionId, Options) ->
    validate_session_id(SessionId),
    call(#add_session{session_id = SessionId, options = Options}).

-spec get_session_list() -> [#esprink_session{}].
get_session_list() ->
    call(#get_session_list{}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    SessionManagerPid = self(),
    {ok, SessionSup} = esprink_sessions_sup:start_link(SessionManagerPid),
    {ok, #state{sessions_sup = SessionSup}}.

handle_call(#add_session{session_id = SessionId, options = Options}, _From, State = #state{sessions_sup = SessionsSup, sessions = Sessions}) ->
    try
        case is_session_exists(SessionId, Sessions) of
            true ->
                io:format("Attempt to add session ~p but such session is already added~n", [SessionId]),
                {reply, {error, already_added}, State};
            false ->
                case esprink_sessions_sup:add_session(SessionsSup, SessionId, Options) of
                    {ok, SessionSupPid} ->
                        SessionSupRef = erlang:monitor(process, SessionSupPid),
                        Sessions2 = do_add_session(SessionId, SessionSupRef, Options, Sessions),
                        io:format("Session ~p was started successful~n", [SessionId]),
                        {reply, ok, State#state{sessions = Sessions2}};
                    Error ->
                        io:format("Cannot start session ~p due error: ~p~n", [SessionId, Error]),
                        {reply, Error, State}
                end
        end
    catch
        _:Exception ->
            io:format("An error: ~p occured while session adding. Session id: ~p~n", [Exception, SessionId]),
            io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
            {reply, {error, Exception}, State}
    end;
handle_call(#get_session_list{}, _From, State = #state{sessions = Sessions}) ->
    io:format("Get session list request~n", []),
    SessionList = get_sessions_info(Sessions),
    {reply, {ok, SessionList}, State};
handle_call(Request, _From, State) ->
    io:format("Unknown request: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', SessionSupRef, process, _, _Reason}, State = #state{sessions = Sessions}) ->
    io:format("Session supervisor ~p is down~n", [SessionSupRef]),
    Sessions2 = remove_session_by_supervisor_ref(SessionSupRef, Sessions),
    {noreply, State#state{sessions = Sessions2}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_session_exists(SessionId, Sessions) ->
    lists:keymember(SessionId, #session.id, Sessions).

do_add_session(SessionId, SessionSupRef, _Options, Sessions) when is_binary(SessionId) ->
    Session = #session{id = SessionId, session_sup_ref = SessionSupRef},
    [Session | Sessions].

remove_session_by_supervisor_ref(SessionSupRef, Sessions) ->
    case lists:keytake(SessionSupRef, #session.session_sup_ref, Sessions) of
        {value, #session{id = Id}, RestSessions} ->
            io:format("Session ~p was removed as hopeless~n", [Id]),
            RestSessions;
        false ->
            io:format("No sessions associated with supervisor ref ~p~n", [SessionSupRef]),
            Sessions
    end.

validate_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
validate_session_id(SessionId) ->
    throw({bad_session_id, SessionId}).

get_sessions_info(Sessions) ->
    [get_session_info(S) || S <- Sessions].

get_session_info(#session{id = Id}) ->
    #esprink_session{id = Id, source_type = file, status = active, options = #{}}.

call(Command) ->
    gen_server:call(?SERVER, Command).