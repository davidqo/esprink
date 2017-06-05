-module(esprink_session_list_handler).

-include("esprink.hrl").

%% Cowboy_http_handler callbacks
-export([
	init/2,
	terminate/3
]).

init(Req, State) ->
	{ok, SessionList} = esprink_session_manager:get_session_list(),
	SessionListJSON = format_session_list(SessionList),
	{ok, Req3} = cowboy_req:reply(200, #{}, SessionListJSON, Req),
	{ok, Req3, State}.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

terminate(_Reason, _Req, _State) ->
	ok.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

format_session_list(SessionList) ->
	Sessions = [format_session(S) || S <- SessionList],
	jsx:encode(Sessions).

format_session(#esprink_session{id = Id, source_type = SourceType, status = Status}) ->
	[
		{<<"id">>, Id},
		{<<"source_type">>, atom_to_binary(SourceType, latin1)},
		{<<"status">>, atom_to_binary(Status, latin1)}
	].
