%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%%
%%%               esprink_session_supervisor
%%%                          |
%%% file_reader --link-- esprink_session ===============> MULTICAST_CLIENTS
%%%                            |  ^
%%%                            |  |
%%%                            |  -retransmit_request-----
%%%                            |                           CLIENT_1
%%%                            ----retransmit_response--->
%%%                                                         ...
%%%
%%%                                                        CLIENT_N
%%%
%%% TODO: This module doesn't implement multicast currently. It sends to remote IP address instead
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_session).

-behaviour(gen_server).

-include("esprink_stream.hrl").

%% API
-export([
    start_link/3
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    id :: binary(),
    session_manager_pid :: pid(),
    %% TODO: IP addresses should not be stored as strings
    local_address :: string(),
    remote_address :: string(),
    local_port :: pos_integer(),
    remote_port :: pos_integer(),
    socket :: term(),
    file_stream_options :: #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessionManagerPid, SessionId, Options) ->
    gen_server:start_link(?MODULE, [SessionManagerPid, SessionId, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SessionManagerPid, SessionId, Options = #{remote_address := RemoteAddress, remote_port := RemotePort}]) ->
    LocalAddress = maps:get(local_address, Options, undefined),
    LocalPort = maps:get(local_port, Options, undefined),
    io:format("Init session. Session manager pid: ~p, session id: ~p, local iface: ~p, local port: ~p, remote iface: ~p, remote port: ~p~n", [SessionManagerPid, SessionId, LocalAddress, LocalPort, RemoteAddress, RemotePort]),
    self() ! init,
    {ok, #state{id = SessionId, session_manager_pid = SessionManagerPid, local_address = LocalAddress, remote_address = RemoteAddress, local_port = LocalPort, remote_port = RemotePort, file_stream_options = Options#{session_id => SessionId}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(#frame{number = Number, body = Body}, State = #state{id = _Id, socket = Socket, remote_port = RemotePort, remote_address = RemoteAddress}) ->
    BinaryFrame = <<Number:64, Body/binary>>,
%%    io:format("Session ~p to received frame with number ~p and send it to ~p~n", [Id, Number, {RemoteAddress, RemotePort}]),
    ok = gen_udp:send(Socket, RemoteAddress, RemotePort, BinaryFrame),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init, State = #state{local_address = LocalAddress, local_port = LocalPort0, file_stream_options = FileStreamOptions}) ->
    SocketOptions = case LocalAddress of
                        undefined ->
                            [];
                        _ ->
                            [{ip, LocalAddress}]
                    end,
    LocalPort = case LocalPort0 of
                    undefined ->
                        0;
                    _ ->
                        LocalPort0
                end,
    io:format("open(~p, ~p)~n", [LocalPort, SocketOptions]),
    {ok, Socket} = gen_udp:open(LocalPort, SocketOptions),
    esprink_stream_reader_file:start_link(self(), FileStreamOptions),
    %% Now we no more need file stream options
    {noreply, State#state{socket = Socket, file_stream_options = undefined}};
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
