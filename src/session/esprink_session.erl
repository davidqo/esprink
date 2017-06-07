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

-record(unicast_udp_stream, {
    local_address :: string(),
    remote_address :: string(),
    local_port :: pos_integer(),
    remote_port :: pos_integer(),
    socket :: term()
}).

-record(multicast_udp_stream, {
    local_address :: string(),
    local_port :: pos_integer(),
    multicast_address :: string(),
    multicast_port :: pos_integer(),
    multicast_ttl :: pos_integer(),
    socket :: term()
}).

-record(state, {
    id :: binary(),
    session_manager_pid :: pid(),
    %% TODO: IP addresses should not be stored as strings
    stream_type :: unicast_udp_stream | multicast_udp_stream,
    stream_descriptor :: #unicast_udp_stream{} | #multicast_udp_stream{},
    source_options :: #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessionManagerPid, SessionId, Options) ->
    gen_server:start_link(?MODULE, [SessionManagerPid, SessionId, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SessionManagerPid, SessionId, Options]) ->
    io:format("Init session. Session manager pid: ~p, session id: ~p", [SessionManagerPid, SessionId]),
    StreamType = maps:get(stream_type, Options, unicast_udp_stream),
    StreamDescriptor = prepare_stream_descriptor(StreamType, SessionId, Options),
    self() ! init,
    {ok, #state{id = SessionId, session_manager_pid = SessionManagerPid, stream_type = StreamType, stream_descriptor = StreamDescriptor, source_options = Options#{session_id => SessionId}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(#frame{number = Number, body = Body}, State = #state{id = _Id, stream_descriptor = StreamDescriptor}) ->
    %%    io:format("Session ~p to received frame with number ~p and send it to ~p~n", [Id, Number, {RemoteAddress, RemotePort}]),
    BinaryFrame = <<Number:64, Body/binary>>,
    StreamDescriptor2 = send_frame(BinaryFrame, StreamDescriptor),
    {noreply, State#state{stream_descriptor = StreamDescriptor2}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init, State = #state{stream_descriptor = StreamDescriptor, source_options = SourceOutions}) ->
    StreamDescriptor2 = initialize_stream(StreamDescriptor),
    esprink_stream_reader_file:start_link(self(), SourceOutions),
    %% Now we no more need file source options
    {noreply, State#state{stream_descriptor = StreamDescriptor2, source_options = undefined}};
handle_info(_Info, State) ->
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

prepare_stream_descriptor(unicast_udp_stream, SessionId, Options = #{remote_address := RemoteAddress, remote_port := RemotePort}) ->
    LocalAddress = maps:get(local_address, Options, undefined),
    LocalPort = maps:get(local_port, Options, undefined),
    io:format("Create unicast udp stream. Session id: ~p, local iface: ~p, local port: ~p, remote iface: ~p, remote port: ~p~n", [SessionId, LocalAddress, LocalPort, RemoteAddress, RemotePort]),
    #unicast_udp_stream{local_address = LocalAddress, local_port = LocalPort, remote_address = RemoteAddress, remote_port = RemotePort};
prepare_stream_descriptor(multicast_udp_stream, SessionId, Options = #{multicast_address := MulticastAddress, multicast_port := MulticastPort}) ->
    LocalAddress = maps:get(local_address, Options, undefined),
    LocalPort = maps:get(local_port, Options, undefined),
    MulticastTTL = maps:get(multicast_ttl, Options, 1),
    io:format("Create multicast udp stream. Session id: ~p, local iface: ~p, local port: ~p, multicast address: ~p, multicast ttl: ~p~n", [SessionId, LocalAddress, LocalPort, MulticastAddress, MulticastTTL]),
    #multicast_udp_stream{local_address = LocalAddress, local_port = LocalPort, multicast_address = MulticastAddress, multicast_port = MulticastPort, multicast_ttl = MulticastTTL}.

initialize_stream(StreamDescriptor = #unicast_udp_stream{local_address = LocalAddress, local_port = LocalPort0}) ->
    SocketOptions = case LocalAddress of
                        undefined ->
                            [];
                        _ ->
                            [{ip, LocalAddress}]
                    end,
    LocalPort = case LocalPort0 of
                    undefined ->
                        0;
                    _ ->
                        LocalPort0
                end,
    io:format("open(~p, ~p)~n", [LocalPort, SocketOptions]),
    {ok, Socket} = gen_udp:open(LocalPort, SocketOptions),
    StreamDescriptor#unicast_udp_stream{socket = Socket};
initialize_stream(StreamDescriptor = #multicast_udp_stream{local_address = LocalAddress, local_port = LocalPort0, multicast_ttl = MulticastTTL}) ->
    SocketOptions = case LocalAddress of
                        undefined ->
                            [];
                        _ ->
                            [{ip, LocalAddress}]
                    end,
    LocalPort = case LocalPort0 of
                    undefined ->
                        0;
                    _ ->
                        LocalPort0
                end,
    %%    {reuseaddr, true}, {ip, Addr}, {add_membership, {Addr, LAddr}
    {ok, Socket} = gen_udp:open(LocalPort, [{multicast_ttl, MulticastTTL} | SocketOptions]),
    StreamDescriptor#multicast_udp_stream{socket = Socket}.

send_frame(BinaryFrame, StreamDescriptor = #unicast_udp_stream{socket = Socket, remote_address = RemoteAddress, remote_port = RemotePort}) ->
    ok = gen_udp:send(Socket, RemoteAddress, RemotePort, BinaryFrame),
    StreamDescriptor#unicast_udp_stream{};
send_frame(BinaryFrame, StreamDescriptor = #multicast_udp_stream{socket = Socket, multicast_address = MulticastAddress, multicast_port = MulticastPort}) ->
    ok = gen_udp:send(Socket, MulticastAddress, MulticastPort, BinaryFrame),
    StreamDescriptor#multicast_udp_stream{}.
