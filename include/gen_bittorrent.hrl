-type payload()      :: binary().
-type piece_id_bin() :: binary().
-type piece_id_int() :: integer().
-type block_id_int() :: integer().

-record(piece_data, {
    piece_index  :: piece_id_int(),
    payload      :: payload(),  % piece payload
    block_offset :: binary(),   % 4 bytes
    length       :: binary()    % 4 bytes
}).

-record(bitfield_data, {
    parsed  :: [{piece_id_int(), boolean()}],
    payload :: payload(),   % raw payload @todo maybe don't need?
    length  :: binary()     % 4 bytes
}).

-record(request_data, {
    piece_index  :: piece_id_int(),
    block_offset :: payload(),  % 4 bytes
    length       :: binary()    % 4 bytes
}).