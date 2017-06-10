%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2017 19:45
%%%-------------------------------------------------------------------
-module(esprink_client).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  retransmit/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(REQUEST_METADATA_TIMEOUT, 5000). %% 5 seconds

-record(metadata, {
  id,
  filename,
  file_size,
  md5_checksum,
  multicast_address,
  retransmit_address,
  retransmit_port
}).

-record(state, {
  local_port :: non_neg_integer(),
  local_address = undefined :: undefined | string(),
  metadata = undefined :: undefined | #metadata{},
  socket
}).

%%%===================================================================
%%% API
%%%===================================================================

retransmit() ->
  gen_server:call(?SERVER, retransmit).

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Options = #{server_url := ServerURL, session_id := SessionId}]) ->
  LocalAddress = maps:get(local_address, Options, undefined),
  LocalPort = maps:get(local_port, Options, 0),
  RequestURL0 = string:join([ServerURL, "session_metadata"], "/"),
  RequestURL = RequestURL0 ++ "?session_id=" ++ characters_to_list(SessionId),
  io:format("REQUEST_URL: ~p~n", [RequestURL]),
  case httpc:request(get, {RequestURL, []}, [{timeout, ?REQUEST_METADATA_TIMEOUT}], [{sync, false}]) of
    {ok, _} ->
      {ok, #state{local_address = LocalAddress, local_port = LocalPort}};
    Error ->
      {stop, Error}
  end.

%%{add_membership, {MulticastGroupAddress, {0,0,0,0}}}

handle_call(retransmit, _From, State = #state{}) ->
  {reply, not_supported, State};
%%handle_call(retransmit, _From, State = #state{server_address = ServerAddress, server_port =
%%ServerPort, socket = Socket}) ->
%%  gen_udp:send(Socket, ServerAddress, ServerPort, <<1:8, 10:64>>),
%%  {reply, ok, State};
handle_call(Request, _From, State) ->
  io:format("Call: ~p~n", [Request]),
  {reply, ok, State}.


handle_cast(Request, State) ->
  io:format("Cast: ~p~n", [Request]),
  {noreply, State}.

handle_info({http, {_, {{_, 200, _}, _, Body}}} = Reply, State = #state{local_port = LocalPort, local_address = LocalAddress}) ->
  io:format("Metadata response: ~p~n", [Body]),
  try parse_metadata(Body) of
    ParsedMetadata = #metadata{multicast_address = MulticastAddress0} ->
      io:format("Parsed metadata: ~p~n", [ParsedMetadata]),
      {ok, MulticastAddress} = inet:parse_address(MulticastAddress0),
      Options = case LocalAddress of
                  undefined ->
                    [];
                  _ ->
                    [{ip, LocalAddress}]
                end,
      gen_udp:open(LocalPort, [binary, inet, {add_membership, {MulticastAddress, {0,0,0,0}}} | Options]),
      {noreply, State}
  catch
    _:Error ->
      io:format("Exception while reading metadata from: ~p~n", [Reply]),
      io:format("Error: ~p. Stacktrace: ~p~n", [Error, erlang:get_stacktrace()]),
      {stop, normal, State}
  end;
handle_info({http, _} = Reply, State) ->
  io:format("Cannot get metadata from reply: ~p", [Reply]),
  {stop, normal, State};
handle_info(Info, State) ->
  io:format("Info: ~p~n", [Info]),
  {noreply, State}.

parse_metadata(Data) ->
  Decoded = jsx:decode(Data),
  FillFun =
    fun
      ({<<"id">>, SessionId}, Meta) ->
        Meta#metadata{id = SessionId};
      ({<<"filename">>, Filename}, Meta) ->
        Meta#metadata{filename = binary_to_list(Filename)};
      ({<<"file_size">>, FileSize}, Meta) ->
        Meta#metadata{file_size =  binary_to_integer(FileSize)};
      ({<<"md5_checksum">>, MD5Checksum}, Meta) ->
        Meta#metadata{md5_checksum =  binary_to_list(MD5Checksum)};
      ({<<"multicast_address">>, MulticastAddress}, Meta) ->
        Meta#metadata{multicast_address = binary_to_list(MulticastAddress)};
      ({<<"retransmit_address">>, RetransmitAddress}, Meta) ->
        Meta#metadata{retransmit_address = binary_to_list(RetransmitAddress)};
      ({<<"retransmit_port">>, RetransmitPort}, Meta) ->
        Meta#metadata{retransmit_port = binary_to_integer(RetransmitPort)};
      (_, Meta) ->
        Meta
    end,
  lists:foldl(FillFun, #metadata{}, Decoded).

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

characters_to_list(Value) when is_binary(Value) ->
  binary_to_list(Value);
characters_to_list(Value) ->
  Value.
