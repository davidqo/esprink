%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_session_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %%% Session registry process. Launches, manages and terminates sessions
    SessionManagerSpec = ?CHILD(esprink_session_manager, worker),
    {ok, {{one_for_one, 5, 10}, [SessionManagerSpec]}}.