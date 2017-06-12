%%%%%-------------------------------------------------------------------
%%%%% @author Dmitry Davidenko
%%%%% @copyright (C) 2017
%%%%% @doc
%%%%%
%%%%% @end
%%%%% Created : 09. Jun 2017 19:45
%%%%%-------------------------------------------------------------------

-module(esprink_client_buffer).

-export([
    init/4,
    process_frame/4,
    get_missed_frames/1,
    terminate/1
]).

-record(frame, {
  sn, %% sequence number
  n, %% number
  data
}).

-record(buffer, {
    frames = [],
    missed_frames = [],
    lwf_sn = 0, %% last_written_frame_sn
    fwf_n = undefined, %% first_written_frame_number (in terms least)
    sn = 0, %% sequence_number
    size_written = 0,
    filename :: string(),
    fd :: term(),
    loop_occured = false :: boolean(),
    expected_size,
    expected_checksum
}).

%% SequenceNumber - sequence number of the first received packet
init(SequenceNumber, Filename, ExpectedSize, ExpectedChecksum) ->
    {ok, Fd} = file:open(Filename ++ ".part1", [write]),
    #buffer{sn = SequenceNumber - 1, filename = Filename, fd = Fd, expected_size = ExpectedSize, expected_checksum = ExpectedChecksum, lwf_sn = SequenceNumber - 1}.

-spec process_frame(FrameSn :: pos_integer(), FrameNumber :: pos_integer(), FrameData :: binary(), #buffer{}) -> {ok, Buffer} | {success, Buffer} | {failure, Buffer}.
process_frame(FrameSN, FrameNumber, FrameData, Buffer = #buffer{frames = Frames, missed_frames = MissedFrames, sn = SN, lwf_sn = LWF}) ->
    Frame = #frame{sn = FrameSN, n = FrameNumber, data = FrameData},
    Buffer2 =
        case FrameSN - SN of
            %% This is the next frame
            1 ->
                %%io:format("Next frame received ~p~n", [FrameSN]),
                Buffer#buffer{frames = store_frame(Frame, Frames), missed_frames = MissedFrames -- [FrameSN], sn = FrameSN};
            %% There are missed frames
            X when X > 1 ->
                NewMissedFrames = lists:seq(SN + 1, FrameSN - 1),
                %%io:format("There are missed frames: ~w~n", [NewMissedFrames]),
                Buffer#buffer{frames = store_frame(Frame, Frames), missed_frames = lists:umerge([NewMissedFrames, MissedFrames]), sn = FrameSN};
            %% Retransmission
            _ ->
                case LWF >= FrameSN of
                    %% This frame was already written
                    true ->
                        %%io:format("Frame ~p already was processed. LWF ~p~n", [FrameSN, LWF]),
                        Buffer;
                    false ->
                        io:format("Frame ~p was retransmitted. LWF ~p~n", [FrameSN, LWF]),
                        Buffer#buffer{frames = store_frame(Frame, Frames), missed_frames = MissedFrames -- [FrameSN]}
                end
        end,
    %% io:format("Frames: ~p~n", [Buffer2#buffer.frames]),
    try_write_frames(Buffer2).

%% Expected amount of bytes was written
try_write_frames(Buffer = #buffer{size_written = SizeWritten, expected_size = ExpectedSize, loop_occured = LoopOccured, filename = Filename, fd = FdBegin, expected_checksum = ExpectedChecksum}) when SizeWritten >= ExpectedSize ->
    %% If loop is not occured, output would be single file
    %% 2 files otherwise
    io:format("Expected amount of bytes was written. Written: ~p, expected: ~p~n", [SizeWritten, ExpectedSize]),
    case LoopOccured of
        true ->
            io:format("Combine result partial files in a single file~n", []),
            {ok, FdEnd} = file:open(Filename ++ ".part1", [read]),
            Ret = file:copy(FdEnd, FdBegin),
            io:format("Copy result: ~p~n", [Ret]),
            file:sync(FdBegin),
            file:close(FdEnd),
            file:close(FdBegin),
            file:delete(Filename ++ ".part1"),
            file:rename(Filename ++ ".part2", Filename);
        false ->
            io:format("Rename single partial file~n", []),
            file:close(FdBegin),
            file:rename(Filename ++ ".part1", Filename)
    end,
    case esprink_file_utils:calculate_md5(Filename) of
        MD5 when MD5 == ExpectedChecksum ->
            io:format("Checksum correct!~n", []),
            {success, Buffer};
        MD5 ->
            io:format("Checksum is incorrect... Checksum ~p. Expected checksum ~p~n", [MD5, ExpectedChecksum]),
            {failure, Buffer}
    end;
%% No frames to write
try_write_frames(Buffer = #buffer{frames = []}) ->
    io:format("No more frames to write~n", []),
    {ok, Buffer};
try_write_frames(Buffer = #buffer{frames = [F = #frame{sn = SN} | T], lwf_sn = LWF}) ->
    case SN - LWF of
        %% This is next frame. Let's write it
        1 ->
            io:format("Write frame: ~p~n", [SN]),
            try_write_frames(do_write(F, Buffer#buffer{frames = T}));
        _ ->
            io:format("There is hole between frames. Waiting for retransmit. Least stored frame SN: ~p, LWF: ~p~n", [SN, LWF]),
            {ok, Buffer}
    end.

do_write(#frame{n = N, sn = SN, data = Data}, Buffer = #buffer{fwf_n = undefined, fd = Fd, size_written = Size}) ->
    ok = file:write(Fd, Data),
    Buffer#buffer{fwf_n = N, lwf_sn = SN, size_written = Size + byte_size(Data)};
do_write(#frame{n = N, sn = SN, data = Data}, Buffer = #buffer{fwf_n = FWF, filename = Filename, fd = Fd, size_written = Size}) ->
    case FWF > N of
        %% It seems that loop occured. Let's write output to another file
        true ->
            file:close(Fd),
            {ok, Fd2} = file:open(Filename ++ ".part2", [write]),
            ok = file:write(Fd2, Data),
            Buffer#buffer{fwf_n = N, lwf_sn = SN, fd = Fd2, size_written = Size + byte_size(Data), loop_occured = true};
        false ->
            ok = file:write(Fd, Data),
            Buffer#buffer{fwf_n = N, lwf_sn = SN, size_written = Size + byte_size(Data)}
    end.

store_frame(Frame, Frames) ->
  do_store_frame(Frame, Frames, []).

%% F - frame
%% SN - sequence number
%% Store frame keeping ordering
do_store_frame(F, [], Acc) ->
  lists:reverse([F | Acc]);
%% Keep searching place to insert
do_store_frame(F1 = #frame{sn = SN1}, [F2 = #frame{sn = SN2} | T], Acc) when (SN1 > SN2) ->
  do_store_frame(F1, T, [F2 | Acc]);
%% Insert before frame
do_store_frame(F1 = #frame{sn = SN1}, [F2 = #frame{sn = SN2} | T], Acc) when (SN1 < SN2) ->
  lists:reverse([F2, F1 | Acc]) ++ T;
%% Drop already existing frame
do_store_frame(#frame{sn = SN1}, List = [#frame{sn = SN2} | _], Acc) when (SN1 == SN2) ->
  lists:reverse(Acc) ++ List.

get_missed_frames(#buffer{missed_frames = MissedFrames}) ->
    MissedFrames.

%% Forget all frames. Used in case of sensible desync
terminate(#buffer{fd = Fd}) ->
    file:close(Fd),
    ok.