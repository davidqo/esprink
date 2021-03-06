-record(stream_info, {
    session_id :: binary(),
    info = #{}
}).

-record(frame, {
    number :: pos_integer(),
    body :: binary()
}).

-record(retransmit, {
    frame_number :: pos_integer(),
    address :: term(),
    sequence_number :: pos_integer()
}).

-record(retransmit_result, {
    frame :: #frame{},
    address :: term(),
    sequence_number :: pos_integer()
}).
