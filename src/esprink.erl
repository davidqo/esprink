%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% TODO: Добавить логгер
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink).

-export([
    start/0,
    add_session/2
]).

start() ->
    esprink_app:start().

add_session(SessionId, Options) ->
    esprink_session_manager:add_session(SessionId, Options).