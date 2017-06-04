%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% Behaviour based on gen_server timeout and allows the user to start the timer
%%% in functional manner. This timer is not cancelled by any message handling
%%% unlike gen_server timeout.
%%% NOTE: This timer conflict with default gen_server timeout behaviour and this
%%% timers will cancel each other
%%% TODO: This is temporary plug. esprink_gen_timer_server must work with micro-seconds.
%%%       Precise of milliseconds is insufficient
%%% TODO: Add multiple timers support
%%% @end
%%% Created : 04. Jun 2017 16:02
%%%-------------------------------------------------------------------
-module(esprink_gen_timer_server).
-author("davidqo").

-behaviour(gen_server).

%% API
-export([
    start_link/3,
    start_link/4
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
    module :: atom(),
    user_state :: term(),
    timeout :: non_neg_integer(),
    start_ts :: erlang:timestamp()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Mod, Args, Options) ->
    gen_server:start_link(esprink_gen_timer_server, [Mod | Args], Options).

start_link(Name, Mod, Args, Options) ->
    gen_server:start_link(Name, esprink_gen_timer_server, [Mod | Args], Options).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Mod | Args]) ->
    case Mod:init(Args) of
        {ok, UserState} ->
            {ok, #state{module = Mod, user_state = UserState}};
        {ok, UserState, Timeout} ->
            {ok, #state{module = Mod, user_state = UserState}, Timeout};
        Ret ->
            Ret
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State = #state{module = Module, user_state = UserState}) ->
    Result = Module:handle_call(Request, From, UserState),
    case Result of
        {{start_timer, Timeout}, UserResult} ->
            inject_timer(Timeout, State, UserResult);
        _ ->
            inject_result(State, Result)
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State = #state{module = Module, user_state = UserState}) ->
    Result = Module:handle_cast(Request, UserState),
    case Result of
        {{start_timer, Timeout}, UserResult} ->
            inject_timer(Timeout, State, UserResult);
        _ ->
            inject_result(State, Result)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, State = #state{module = Module, user_state = UserState}) ->
    State2 = State#state{timeout = undefined, start_ts = undefined},
    Result = Module:handle_info(timeout, UserState),
    case Result of
        {{start_timer, Timeout}, UserResult} ->
            inject_timer(Timeout, State2, UserResult);
        _ ->
            inject_result(State2, Result)
    end;
handle_info(Info, State = #state{module = Module, user_state = UserState}) ->
    Result = Module:handle_info(Info, UserState),
    case Result of
        {{start_timer, Timeout}, UserResult} ->
            inject_timer(Timeout, State, UserResult);
        _ ->
            inject_result(State, Result)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, #state{module = Module, user_state = UserState}) ->
    Module:terminate(Reason, UserState).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(OldVsn, #state{module = Module, user_state = UserState}, Extra) ->
    Module:code_change(OldVsn, UserState, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================

inject_timer(Timeout, State, UserResult) ->
    State2 = State#state{timeout = Timeout, start_ts = erlang:timestamp()},
    inject_result(State2, UserResult).

inject_result(State = #state{start_ts = undefined}, UserResult) ->
    case UserResult of
        {reply, Reply, UserState} ->
            {reply, Reply, State#state{user_state = UserState}};
        {reply, Reply, UserState, Timeout} ->
            {reply, Reply, State#state{user_state = UserState}, Timeout};
        {noreply, UserState} ->
            {noreply, State#state{user_state = UserState}};
        {noreply, UserState, Timeout} ->
            {noreply, State#state{user_state = UserState}, Timeout};
        {stop, Reason, UserState} ->
            {stop, Reason, State#state{user_state = UserState}};
        {stop, Reason, Reply, UserState} ->
            {stop, Reason, Reply, State#state{user_state = UserState}}
    end;
inject_result(State = #state{start_ts = StartTS, timeout = Timeout}, UserResult) ->
    TimeElapsed = timer:now_diff(erlang:timestamp(), StartTS) div 1000,
    TimeToSleep = case TimeElapsed of
                      _ when TimeElapsed >= Timeout ->
                          0;
                      TimeElapsed ->
                          Timeout - TimeElapsed
                  end,
    case UserResult of
        {reply, Reply, UserState} ->
            {reply, Reply, State#state{user_state = UserState}, TimeToSleep};
        {reply, Reply, UserState, _} ->
            {reply, Reply, State#state{user_state = UserState}, TimeToSleep};
        {noreply, UserState} ->
            {noreply, State#state{user_state = UserState}, TimeToSleep};
        {noreply, UserState, _} ->
            {noreply, State#state{user_state = UserState}, TimeToSleep};
        {stop, Reason, UserState} ->
            {stop, Reason, State#state{user_state = UserState}};
        {stop, Reason, Reply, UserState} ->
            {stop, Reason, Reply, State#state{user_state = UserState}}
    end.

