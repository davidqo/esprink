-module(esprink_index_handler).

%% Cowboy_http_handler callbacks
-export([
	init/2,
	terminate/3
]).

init(Req, State) ->
	{ok, HTML} = index_dtl:render([]),
	{ok, Req3} = cowboy_req:reply(200, #{}, HTML, Req),
	{ok, Req3, State}.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

terminate(_Reason, _Req, _State) ->
	ok.
%%+++++++++++++++++++++++++++++++++++++++++++++++++