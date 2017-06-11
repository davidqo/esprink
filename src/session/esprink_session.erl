%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%%
%%%               esprink_session_supervisor
%%%                          |
%%% file_reader --link-- esprink_session ===============> MULTICAST_CLIENTS
%%%                            |  ^
%%%                            |  |
%%%                            |  -retransmit_request-----
%%%                            |                           CLIENT_1
%%%                            ----retransmit_response--->
%%%                                                         ...
%%%
%%%                                                        CLIENT_N
%%%
%%% TODO: This module doesn't implement multicast currently. It sends to remote IP address instead
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_session).

-behaviour(gen_server).

-include("esprink_stream.hrl").

-define(RETRANSMIT_CODE, 1).


%% API
-export([
    start_link/3
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    id :: binary(),
    session_manager_pid :: pid(),
    %% TODO: IP addresses should not be stored as strings
    local_address :: string(),
    remote_address :: string(),
    local_port :: pos_integer(),
    remote_port :: pos_integer(),
    multicast_ttl :: pos_integer(),
    socket :: term(),
    file_stream_options :: #{},
    file_stream_pid :: pid(),
    sequence_number = 1 :: pos_integer(),
    first_frame_number = undefined :: undefined | pos_integer(),
    current_frame_number = undefined :: undefined | pos_integer(),
    last_frame_number = undefined :: undefined | pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessionManagerPid, SessionId, Options) ->
    gen_server:start_link(?MODULE, [SessionManagerPid, SessionId, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SessionManagerPid, SessionId, Options = #{remote_address := RemoteAddress, remote_port := RemotePort}]) ->
    LocalAddress0 = maps:get(local_address, Options, undefined),
    LocalAddress = case LocalAddress0 of
                       undefined ->
                           get_server_local_address();
                       _ ->
                           LocalAddress0
                   end,
    LocalPort = maps:get(local_port, Options, 0),
    MulticastTTL = maps:get(multicast_ttl, Options, 1),
    io:format("Init session. Session manager pid: ~p, session id: ~p, local iface: ~p, local port: ~p, remote iface: ~p, remote port: ~p~n", [SessionManagerPid, SessionId, LocalAddress, LocalPort, RemoteAddress, RemotePort]),
    gen_server:cast(self(), init),
    {ok, #state{id = SessionId, session_manager_pid = SessionManagerPid, local_address = LocalAddress, remote_address = RemoteAddress, local_port = LocalPort, remote_port = RemotePort, multicast_ttl = MulticastTTL, file_stream_options = Options#{session_id => SessionId}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, State = #state{local_address = LocalAddress0, local_port = LocalPort0, multicast_ttl = MulticastTTL, file_stream_options = FileStreamOptions}) ->
    {ok, LocalAddress} = inet:parse_address(LocalAddress0),
    {ok, Socket} = gen_udp:open(LocalPort0, [{multicast_ttl, MulticastTTL}, {active, once}, binary, {ip, LocalAddress}]),
    %% if local port was 0, we can now know its actual values
    {ok, LocalPort} = inet:port(Socket),
    {ok, FileStreamPid} = esprink_stream_reader_file:start_link(self(), FileStreamOptions),
    %% Now we no more need file stream options
    {noreply, State#state{socket = Socket, file_stream_options = undefined, file_stream_pid = FileStreamPid, local_port = LocalPort}};
handle_cast(Frame = #stream_info{info = Info}, State = #state{id = Id, session_manager_pid = SessionManagerPid, local_address = LocalAddress, local_port = LocalPort}) ->
    io:format("Session ~p stream info received: ~p. Transfer it to session manager~n", [Id, Info]),
    %% We should update local address in session manager if it is not set yet
    gen_server:cast(SessionManagerPid, Frame#stream_info{info = Info#{local_address => LocalAddress, local_port => LocalPort}}),
    {noreply, State};
handle_cast(Frame = #frame{}, State = #state{id = _Id, socket = Socket, remote_port = RemotePort, remote_address = RemoteAddress}) ->
    State2 = #state{sequence_number = SequenceNumber} = update_sequence_info(Frame, State),
    send_frame(Socket, RemoteAddress, RemotePort, SequenceNumber, Frame),
    {noreply, State2};
handle_cast(#retransmit_result{frame = Frame, address = {RemoteAddress, RemotePort}, sequence_number = SequenceNumber}, State = #state{socket = Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    send_frame(Socket, RemoteAddress, RemotePort, SequenceNumber, Frame),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, _, Address, Port, << ?RETRANSMIT_CODE:8, SequenceNumber:64>>}, State = #state{file_stream_pid = FileStreamPid}) ->
    %% For simplicity process only single retransmit request at once.
    %% It provided by {active, once} option.`
    FrameNumber = frame_number_by_sequence_number(SequenceNumber, State),
    gen_server:cast(FileStreamPid, #retransmit{frame_number = FrameNumber, address = {Address, Port}, sequence_number = SequenceNumber}),
    {noreply, State#state{}};
handle_info(Info, State = #state{socket = Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    io:format("Unknown handle_info message: ~p", [Info]),
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_frame(Socket, RemoteAddress, RemotePort, SequenceNumber, #frame{number = Number, body = Body}) ->
    BinaryFrame = <<SequenceNumber:64, Number:64, Body/binary>>,
%%    io:format("Session ~p to received frame with number ~p and send it to ~p~n", [Id, Number, {RemoteAddress, RemotePort}]),
    ok = gen_udp:send(Socket, RemoteAddress, RemotePort, BinaryFrame).

get_server_local_address() ->
    {ok, List} = inet:getif(),
    {IpAddr, _, _} = hd(List),
    Tokens = [integer_to_list(X) || X <- tuple_to_list(IpAddr)],
    string:join(Tokens, ".").

%% First frame
update_sequence_info(#frame{number = Number}, State = #state{first_frame_number = undefined}) ->
    State#state{first_frame_number = Number, current_frame_number = Number, sequence_number = 1};
update_sequence_info(#frame{number = Number}, State = #state{first_frame_number = FirstFrameNumber, current_frame_number = PrevFrameNumber, sequence_number = SequenceNumber}) ->
    State2 = case Number of
                %% Streaming was started from the beginning
                _ when (Number == FirstFrameNumber) ->
                    State#state{current_frame_number = Number, last_frame_number = PrevFrameNumber};
                _ ->
                    State#state{current_frame_number = Number}
            end,
    State2#state{sequence_number = SequenceNumber + 1}.

frame_number_by_sequence_number(SequenceNumber, #state{first_frame_number = FirstFrameNumber, last_frame_number = LastFrameNumber}) ->
    %% Stream could be started not from frame_number = 1
    Offset = SequenceNumber + (FirstFrameNumber - 1),
    case LastFrameNumber of
        %% Last frame not yet discovered
        undefined ->
            Offset;
        _ ->
            Offset rem LastFrameNumber
    end.

