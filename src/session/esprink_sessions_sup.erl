%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% sessions_sup -----------
%%%      |                 |
 %%      |                 |
%%%      |                 |
%%% session_sup_1  ... session_sup_N
%%%
%%%
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_sessions_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/1,
    add_session/3
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(SessionManagerPid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SessionManagerPid]).

add_session(SupervisorPid, SessionId, Options) ->
    io:format("Add session api call. Supervisor pid: ~p, session id: ~p, options: ~p~n", [SupervisorPid, SessionId, Options]),
    supervisor:start_child(SupervisorPid, [SessionId, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(SessionManagerPid) ->
    SessionSupSpec = ?CHILD(esprink_session_sup, supervisor, SessionManagerPid),
    {ok, {{simple_one_for_one, 5, 10}, [SessionSupSpec]}}.