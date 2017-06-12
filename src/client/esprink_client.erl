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
  start_link_from_cli/0,
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

-define(LOST_AMOUNT_CAUSE_DESYNC, 30).

-define(RETRANSMIT_CODE, 1).

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
  socket,
  lost_percentage = 0,
  from_cli = false :: boolean(),
  owner_ref = undefined :: undefined | pid(),
  buffer = undefined :: undefined | term()
}).

%%%===================================================================
%%% API
%%%===================================================================

retransmit() ->
  gen_server:call(?SERVER, retransmit).

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

start_link_from_cli() ->
  try
    [ServerUrl, SessionId | OptionalArgs] = init:get_plain_arguments(),
    Options0 = #{server_url => ServerUrl, session_id => list_to_binary(SessionId), from_cli => true, owner_ref => self()},
    Options = parse_optional_args(Options0, OptionalArgs),
    inets:start(),
    case start_link(Options) of
      {ok, _} ->
        io:format("Connecting to server...~n", []);
      {error, Error} ->
        io:format("Error occured: ~p~n", [Error]),
        halt(0)
    end,
    receive
      {start_result, success} ->
        io:format("Client successfully started~n", []);
      {start_result, failure} ->
        io:format("Client start failed...~n", []),
        halt(0)
    end
  catch
    _:_ ->
      io:format("Bad command args~n", []),
      print_help(),
      halt(0)
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Options = #{server_url := ServerURL, session_id := SessionId}]) ->
  LocalAddress = maps:get(local_address, Options, undefined),
  LocalPort = maps:get(local_port, Options, 0),
  LostPercentage = maps:get(lost_percentage, Options, 0),
  FromCLI = maps:get(from_cli, Options, false),
  OwnerRef = maps:get(owner_ref, Options, undefined),
  RequestURL0 = string:join([ServerURL, "session_metadata"], "/"),
  RequestURL = RequestURL0 ++ "?session_id=" ++ characters_to_list(SessionId),
  io:format("REQUEST_URL: ~p~n", [RequestURL]),
  case httpc:request(get, {RequestURL, []}, [{timeout, ?REQUEST_METADATA_TIMEOUT}], [{sync, false}]) of
    {ok, _} ->
      {ok, #state{local_address = LocalAddress, local_port = LocalPort, lost_percentage = LostPercentage, from_cli = FromCLI, owner_ref = OwnerRef}};
    Error ->
      {stop, Error}
  end.

handle_call(Request, _From, State) ->
  io:format("Call: ~p~n", [Request]),
  {reply, ok, State}.


handle_cast(Request, State) ->
  io:format("Cast: ~p~n", [Request]),
  {noreply, State}.

%% Metadata response
handle_info({http, {_, {{_, 200, _}, _, Body}}} = Reply, State = #state{local_port = LocalPort, local_address = LocalAddress, from_cli = FromCli, owner_ref = OwnerRef}) ->
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
      {ok, Socket} = gen_udp:open(LocalPort, [binary, inet, {add_membership, {MulticastAddress, {0,0,0,0}}} | Options]),
      try_notify_success(FromCli, OwnerRef),
      {noreply, State#state{metadata = ParsedMetadata, socket = Socket}}
  catch
    _:Error ->
      io:format("Exception while reading metadata from: ~p~n", [Reply]),
      io:format("Error: ~p. Stacktrace: ~p~n", [Error, erlang:get_stacktrace()]),
      try_notify_failure(FromCli, OwnerRef),
      {stop, normal, State}
  end;
%% Bad metadata response
handle_info({http, _} = Reply, State = #state{from_cli = FromCli, owner_ref = OwnerRef}) ->
  io:format("Cannot get metadata from reply: ~p~n", [Reply]),
  try_notify_failure(FromCli, OwnerRef),
  {stop, normal, State};
%% Initial frame
handle_info({udp, _, _, _, Data}, State = #state{lost_percentage = LostPercentage}) ->
  <<SequenceNumber:64, FrameNumber:64, FrameData/binary>> = Data,
  case LostPercentage of
    0 ->
      process_frame(SequenceNumber, FrameNumber, FrameData, State);
    _ ->
      case rand:uniform(100) of
        %% Packet lost
        X when (X =< LostPercentage) ->
          io:format("Drop frame seq: ~p num: ~p data: ~p~n", [SequenceNumber, FrameNumber, FrameData]),
          {noreply, State};
        _ ->
          io:format("Process frame seq: ~p num: ~p data: ~p~n", [SequenceNumber, FrameNumber, FrameData]),
          process_frame(SequenceNumber, FrameNumber, FrameData, State)
      end
  end;
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

%%Initial frame
process_frame(SequenceNumber, FrameNumber, FrameData, State = #state{buffer = undefined, metadata = #metadata{filename = Filename, file_size = FileSize, md5_checksum = Checksum}}) ->
  Buffer = esprink_client_buffer:init(SequenceNumber, Filename, FileSize, Checksum),
  process_frame(SequenceNumber, FrameNumber, FrameData, State#state{buffer = Buffer});
process_frame(SequenceNumber, FrameNumber, FrameData, State = #state{buffer = Buffer}) ->
  case esprink_client_buffer:process_frame(SequenceNumber, FrameNumber, FrameData, Buffer) of
    {ok, Buffer2} ->
      MissedFrames = esprink_client_buffer:get_missed_frames(Buffer2),
      MissedFramesCount = length(MissedFrames),
      case length(MissedFrames) > ?LOST_AMOUNT_CAUSE_DESYNC of
        %% To much missed frames
        true ->
          io:format("Too much missed frames: ~p. It's looks like desync", [MissedFramesCount]),
          {stop, normal, State#state{buffer = Buffer2}};
        false ->
          [retransmit(SN, State) || SN <- MissedFrames],
          {noreply, State#state{buffer = Buffer2}}
      end;
    {success, Buffer2} ->
      {stop, normal, State#state{buffer = Buffer2}};
    {failure, Buffer2} ->
      {stop, normal, State#state{buffer = Buffer2}}
  end.

characters_to_list(Value) when is_binary(Value) ->
  binary_to_list(Value);
characters_to_list(Value) ->
  Value.

parse_optional_args(Options, []) ->
  Options;
parse_optional_args(Options, ["-l", LocalAddress | Tail]) ->
  parse_optional_args(Options#{local_address => LocalAddress}, Tail);
parse_optional_args(Options, ["-p", LocalPort | Tail]) ->
  parse_optional_args(Options#{local_port => list_to_integer(LocalPort)}, Tail);
parse_optional_args(Options, ["-L", LostPercentage | Tail]) ->
  parse_optional_args(Options#{lost_percentage => list_to_integer(LostPercentage)}, Tail).

print_help() ->
  io:format(
    "esprink_client <ServerURL> <SessionId> [-l <LocalAddress>] [-p <LocalPort>] [-L <LostPercentage>]~n"
    "\tLostPercentage - If specified client will simulate packet lost for retransmission mechanism testing. By default 0~n",
    []).

try_notify_success(true, OwnerRef) ->
  OwnerRef ! {start_result, success};
try_notify_success(false, _) ->
  ok.

try_notify_failure(true, OwnerRef) ->
  OwnerRef ! {start_result, failure};
try_notify_failure(false, _) ->
  ok.

retransmit(SequenceNumber, #state{socket = Socket, metadata = #metadata{retransmit_address = RetransmitAddress, retransmit_port = RetransmitPort}}) ->
  io:format("Retransmit request. Sequence number: ~p to ~p:~p~n", [SequenceNumber, RetransmitAddress, RetransmitPort]),
  gen_udp:send(Socket, RetransmitAddress, RetransmitPort, << ?RETRANSMIT_CODE:8, SequenceNumber:64>>),
  ok.