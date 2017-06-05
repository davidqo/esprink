-module(esprink_web_server_utils).

-export([
	get_request_info/2,
	get_request_body/1,
	get_value/2,
	get_value/3,
	encode/1
]).

get_request_body(Request) ->
	cowboy_req:parse_qs(Request).
%%+++++++++++++++++++++++++++++++++++++++++++++++++

get_value(Key, Body) ->
	get_value(Key, Body, undefined).
%%+++++++++++++++++++++++++++++++++++++++++++++++++

get_value(Key, Body, Default) ->
	proplists:get_value(Key, Body, Default).
%%+++++++++++++++++++++++++++++++++++++++++++++++++

get_request_info(url, Req) ->
	cowboy_req:uri(Req).
%%+++++++++++++++++++++++++++++++++++++++++++++++++

encode(JsxData) ->
	jsx:encode(JsxData).
%%+++++++++++++++++++++++++++++++++++++++++++++++++