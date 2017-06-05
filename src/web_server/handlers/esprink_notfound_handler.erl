-module(esprink_notfound_handler).

%% Cowboy_http_handler callbacks
-export([
	init/2,
	terminate/3
]).

init(Req, State) ->
	URL = esprink_web_server_utils:get_request_info(url, Req),
	{ok, HTML} = '404_dtl':render([{url, URL}]),
	{ok, Req3} = cowboy_req:reply(404, #{}, HTML, Req),
	{ok, Req3, State}.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

terminate(_Reason, _Req, _State) ->
	ok.
%%+++++++++++++++++++++++++++++++++++++++++++++++++