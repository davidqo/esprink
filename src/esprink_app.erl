%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_app).

-behaviour(application).

%% Application callbacks
-export([
    start/0,
    start/2,
    stop/0,
    stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start() ->
    application:ensure_all_started(esprink),
    application:start(esprink).

start(_StartType, _StartArgs) ->
    case esprink_app_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------

stop() ->
    stop(normal).

-spec(stop(State :: term()) -> term()).
stop(_State) ->
    esprink_app_sup:stop().

%%%===================================================================
%%% Internal functions
%%%===================================================================
