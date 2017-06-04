%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 8:58
%%%-------------------------------------------------------------------

-module(esprink_app_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    exit(?SERVER, normal).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EsprinkSessionManagerSupSpec = ?CHILD(esprink_session_manager_sup, supervisor),
    {ok, { {one_for_one, 5, 10}, [EsprinkSessionManagerSupSpec]} }.