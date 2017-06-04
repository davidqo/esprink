%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 2000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(SessionManagerPid, SessionId, Options) ->
    supervisor:start_link(?MODULE, [SessionManagerPid, SessionId, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    SessionSupSpec = ?CHILD(esprink_session, worker, Args),
    {ok, {{one_for_one, 5, 10}, [SessionSupSpec]}}.