%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 04. Jun 2017 9:10
%%%-------------------------------------------------------------------
-module(esprink_stream_reader_file).

-behaviour(esprink_gen_timer_server).

-define(DEFAULT_MAX_CHUNK_SIZE, 1200). %% 1200 bytes

%% API
-export([
    start_link/2
]).

-include("esprink_stream.hrl").

-define(FILE_EMPTY_TIMEOUT, 5000). %% 5 seconds

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    session_pid :: pid(),
    filename :: nonempty_string(),
    bytes_per_sec :: pos_integer(),
    max_chunk_size :: pos_integer(),
    current_frame = 1,
    fd :: term(),
    %% Read interval between frames
    transmission_interval :: pos_integer(),
    preferable_chunk_size :: pos_integer(),
    %% Only for debugging
    session_id
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SessionPid, Options) ->
    esprink_gen_timer_server:start_link(?MODULE, [SessionPid, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SessionPid, Options = #{filename := Filename, bytes_per_sec := BytesPerSec, session_id := SessionId}]) ->
    MaxChunkSize = maps:get(max_chunk_size, Options, ?DEFAULT_MAX_CHUNK_SIZE),
    io:format("Init file stream reader process linked with session ~p(~p). Filename: ~p, rate bytes per second: ~p, max chunk size: ~p ~n", [SessionId, SessionPid, Filename, BytesPerSec, MaxChunkSize]),
    gen_server:cast(self(), init),
    {ok, #state{session_pid = SessionPid, filename = Filename, bytes_per_sec = BytesPerSec, max_chunk_size = MaxChunkSize, session_id = SessionId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, State = #state{filename = Filename, current_frame = CurrentFrame, bytes_per_sec = BytesPerSec, max_chunk_size = MaxChunkSize, session_id = SessionId, session_pid = SessionPid}) ->
    ChunksPerSecond = calculate_chunks_per_second(BytesPerSec, MaxChunkSize),
    %% If we always transmit packet in MaxChunkSize the last chunk in second-interval may be much smaller
    %% then others. To avoid it we will always transmit chunk of smaller then MaxChunkSize called PreferedPacketSize.
    %% So all packets in second-interval, including the last one, will be the same size
    PreferableChunkSize = trunc(BytesPerSec / ChunksPerSecond),
    TransmissionInterval = 1000 div ChunksPerSecond,
    %%% Stream can start not only from the beginning of the file.
    PositionInBytes = CurrentFrame * PreferableChunkSize,
    MD5Checksum = esprink_file_utils:calculate_md5(Filename),
    FileSize = esprink_file_utils:size(Filename),
    {ok, Fd} = file:open(Filename, [read, binary]),
    {ok, _} = file:position(Fd, PositionInBytes),
    io:format("File stream reader process linked with session ~p(~p) will be started immidiately. Chunks per second: ~p, transmission interval: ~p, preferable chunk size: ~p~n", [SessionId, SessionPid, ChunksPerSecond, TransmissionInterval, PreferableChunkSize]),
    gen_server:cast(SessionPid, #stream_info{session_id = SessionId, info = #{file_size => FileSize, md5_checksum => MD5Checksum}}),
    {{start_timer, 0}, {noreply, State#state{fd = Fd, transmission_interval = TransmissionInterval, preferable_chunk_size = PreferableChunkSize}}};
handle_cast(#retransmit{frame_number = FrameNumber, address = Address, sequence_number = SequenceNumber}, State = #state{preferable_chunk_size = PreferableChunkSize, filename = Filename, session_pid = SessionPid, session_id = SessionId}) ->
    %% For optimisation purposes we should keep this file opened for retransmission needs
    {ok, FdOnce} = file:open(Filename, [read, binary]),
    Offset = (FrameNumber - 1) * PreferableChunkSize,
    {ok, _} = file:position(FdOnce, Offset),
    case file:read(FdOnce, PreferableChunkSize) of
        {ok, Data} ->
            Result = #retransmit_result{frame = #frame{number = FrameNumber, body = Data}, address = Address, sequence_number = SequenceNumber},
            gen_server:cast(SessionPid, Result),
            file:close(FdOnce),
            {noreply, State};
        Error ->
            io:format("[ERROR] Session: ~p. Cannot retransmit frame due error: ~p", [SessionId, Error]),
            file:close(FdOnce),
            {noreply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State = #state{fd = Fd, current_frame = CurrentFrame, session_pid = SessionPid, session_id = SessionId, transmission_interval = TransmissionInterval, preferable_chunk_size = PreferableChunkSize}) ->
%%    io:format("File stream ~p(~p) timeout occured. Current frame: ~p~n", [SessionId, SessionPid, CurrentFrame]),
    case file:read(Fd, PreferableChunkSize) of
        {ok, Data} ->
            gen_server:cast(SessionPid, #frame{number = CurrentFrame, body = Data}),
            {{start_timer, TransmissionInterval}, {noreply, State#state{current_frame = CurrentFrame + 1}}};
        %% End of file reached
        eof ->
            case CurrentFrame of
                %% File is empty. Let's await some time
                1 ->
                    io:format("File stream ~p(~p). File is empty. Let's await some time~n", [SessionId, SessionPid]),
                    {{start_timer, 5000}, {noreply, State}};
                %% Let's stream from the beginning
                _ ->
                    io:format("File stream ~p(~p). EOF is reached. Let's start from the beginning~n", [SessionId, SessionPid]),
                    {ok, _} = file:position(Fd, 0),
                    {{start_timer, 0}, {noreply, State#state{current_frame = 1}}}
            end
    end;
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

calculate_chunks_per_second(BytesPerSec, MaxChunkSize) when (MaxChunkSize > BytesPerSec) ->
    1;
calculate_chunks_per_second(BytesPerSec, MaxChunkSize) ->
    Chunks0 = BytesPerSec div MaxChunkSize,
    Chunks = case (BytesPerSec rem MaxChunkSize) of
                 0 ->
                     Chunks0;
                 _ ->
                     Chunks0 + 1
             end,
    case Chunks of
        _ when Chunks =< 1000 ->
            Chunks;
        _ ->
            %% TODO: This is temporary plug. We only need to teach esprink_gen_timer_server work with uSeconds
            io:format("To big amount of chunks. Force chunks per second count to 1000!", []),
            1000
    end.


