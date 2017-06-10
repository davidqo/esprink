-module(esprink_session_metadata_handler).

-include("esprink.hrl").

%% Cowboy_http_handler callbacks
-export([
	init/2,
	terminate/3
]).

init(Req, State) ->
	Body = esprink_web_server_utils:get_request_body(Req),
	SessionId = esprink_web_server_utils:get_value(<<"session_id">>, Body),
	 case esprink_session_manager:get_session_info(SessionId) of
		 {ok, SessionInfo} ->
			 SessionMetadataJSON = format_session_metadata(SessionInfo),
			 {ok, Req3} = cowboy_req:reply(200, #{}, SessionMetadataJSON, Req),
			 {ok, Req3, State};
		 no_such_session ->
			 {ok, Req3} = cowboy_req:reply(404, #{}, <<"No such session">>, Req),
			 {ok, Req3, State}
	 end.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

terminate(_Reason, _Req, _State) ->
	ok.
%%+++++++++++++++++++++++++++++++++++++++++++++++++

format_session_metadata(#esprink_session{id = Id, options = #{filename := Filename, md5_checksum := MD5Checksum, file_size := Size, remote_address := RemoteAddress, local_address := RetransmitAddress, local_port := RetransmitPort}}) ->
	JsxData = [
		{<<"id">>, Id},
		{<<"filename">>, list_to_binary(filename:basename(Filename))},
		{<<"file_size">>, integer_to_binary(Size)},
		{<<"md5_checksum">>, list_to_binary(MD5Checksum)},
		{<<"multicast_address">>, list_to_binary(RemoteAddress)},
		{<<"retransmit_address">>, list_to_binary(RetransmitAddress)},
		{<<"retransmit_port">>, integer_to_binary(RetransmitPort)}
	],
	jsx:encode(JsxData).
%%%%+++++++++++++++++++++++++++++++++++++++++++++++++
