%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% gen_bittorrent tests.
%%% @end
%%% Created : 02. Aug 2019 14.33
%%%-------------------------------------------------------------------
-module(gen_bittorrent_tests).
-author("bartimaeus").

-include("gen_bittorrent.hrl").
-include("gen_bittorrent_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Mirrored internal testing process states and state data.
%%%===================================================================

-record(state, {
    handshake       = not_handshaked    :: not_handshaked | handshaked,
    leecher_state   = not_interested    :: not_interested | interested,
    peer_state      = choked            :: unchoked | choked
}).

-record(data, {
    % BitTorrent specific fields
    socket                                              :: port(),
    torrent_hash                                        :: binary(),
    peer_ip                                             :: inet:ip_address(),
    peer_port                                           :: inet:port_number(),
    piece_id                                            :: non_neg_integer(),
    piece_size                                          :: pos_integer(),
    blocks                                              :: [non_neg_integer()],
    blocks_not_requested                                :: [non_neg_integer()],
    connect_timeout         = ?DEFAULT_CONNECT_TIMEOUT  :: integer(),
    rest_payload                                        :: payload(),
    protocol                = ?DEFAULT_PROTOCOL         :: tcp | udp,
    peer_id                                             :: string(),
    request_length          = ?DEFAULT_REQUEST_LENGTH   :: pos_integer(), % 16 kB
    retries                 = ?DEFAULT_RETRIES          :: non_neg_integer(),
    % Process specific fields
    name                                                :: term(),
    cb_mod                                              :: module(),
    cb_state                                            :: term(),
    args                                                :: [term()]
}).


%%%===================================================================
%%% Tests.
%%%===================================================================

request_piece_test_() ->
    OldCbState = {old_cb_state},
    NewCbState = {new_cb_state},
    {setup,
        fun() ->
            ok = meck:new(gen_bittorrent_message, [passthrough]),
            ok = meck:new(gen_bittorrent_cb, [non_strict]),
            ok = meck:expect(gen_bittorrent_message, request_piece, ['_', '_'], ok),
            ok = meck:expect(gen_bittorrent_cb, block_requested, fun(_, _, _, _) -> {ok, NewCbState} end)
        end,
        fun(_) ->
            true = meck:validate([gen_bittorrent_message, gen_bittorrent_cb]),
            ok = meck:unload([gen_bittorrent_message, gen_bittorrent_cb])
        end,
        [{"Request of a next piece.",
            fun() ->
                Data = #data{
                    piece_id                = 1,
                    piece_size              = 49152,
                    blocks_not_requested    = [0, 1, 2, 3],
                    request_length          = 16384,
                    cb_mod                  = gen_bittorrent_cb,
                    cb_state                = OldCbState
                },
                Result = Data#data{
                    cb_state              = NewCbState,
                    blocks_not_requested  = []
                },
                ?assertEqual(
                    Result,
                    gen_bittorrent:request_piece(Data)
                ),
                ?assertEqual(
                    4,
                    meck:num_calls(gen_bittorrent_cb, block_requested, ['_', '_', '_', '_'])
                ),
                ?assertEqual(
                    4,
                    meck:num_calls(gen_bittorrent_message, create_pipeline_request_piece, ['_', '_', '_', '_'])
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_message, request_piece, ['_', '_'])
                )
            end
        }]
    }.


get_request_data_test_() ->
    [
        {"Get increased `length` and `offset` when block is NOT the last one.",
            fun() ->
                ?assertEqual(
                    {ok, {<<0,0,64,0>>, 16384}},
                    gen_bittorrent:get_request_data(1, 50000, 16384)
                )
            end
        },
        {"Get increased `length` and `offset` when block is the last one.",
            fun() ->
                ?assertEqual(
                    {ok, {<<0,0,192,0>>, 848}},
                    gen_bittorrent:get_request_data(3, 50000, 16384)
                )
            end
        }
    ].


get_peer_new_state_test_() ->
    [
        {"Change state from choke to unchoke.",
            fun() ->
                ?assertEqual(
                    unchoked,
                    gen_bittorrent:get_peer_new_state([{handshake, true}, {unchoke, true}], choked)
                )
            end
        },
        {"Change state from unchoke to choke.",
            fun() ->
                ?assertEqual(
                    choked,
                    gen_bittorrent:get_peer_new_state([{choke, true}], unchoked)
                )
            end
        },
        {"Keep current state.",
            fun() ->
                ?assertEqual(
                    unchoked,
                    gen_bittorrent:get_peer_new_state([{handshake, true}], unchoked)
                )
            end
        }
    ].


process_downloaded_block_test_() ->
    OldCbState = {old_cb_state},
    NewCbState = {new_cb_state},
    ParsedPayload = [
        {piece, #piece_data{
            piece_index  = 0,
            payload      = <<0,1,2,3>>,
            block_offset = <<0,0,0,0>>,
            length       = <<0,0,0,4>>
        }},
        {piece, #piece_data{
            piece_index  = 0,
            payload      = <<4,5,6,7>>,
            block_offset = <<0,0,0,4>>,
            length       = <<0,0,0,4>>
        }}
    ],
    Data = #data{
        blocks          = [0, 1, 2, 3],
        request_length  = 4,
        cb_mod          = gen_bittorrent_cb,
        cb_state        = OldCbState
    },
    {setup,
        fun() ->
            ok = meck:new(gen_bittorrent_cb, [non_strict]),
            ok = meck:expect(gen_bittorrent_cb, block_downloaded, fun(_, _, _, _, _) -> {ok, NewCbState} end)
        end,
        fun(_) ->
            true = meck:validate(gen_bittorrent_cb),
            ok = meck:unload(gen_bittorrent_cb)
        end,
        [{"Process empty downloaded and parsed piece data.",
            fun() ->
                ?assertEqual(
                    Data#data{cb_state = {old_cb_state}, blocks = [0,1,2,3]},
                    gen_bittorrent:process_downloaded_block([], Data)
                ),
                ?assertEqual(
                    0,
                    meck:num_calls(gen_bittorrent_cb, block_downloaded, ['_', '_', '_', '_', '_'])
                )
            end
        },
        {"Process downloaded and parsed piece data.",
            fun() ->
                ?assertEqual(
                    Data#data{cb_state = {new_cb_state}, blocks = [2,3]},
                    gen_bittorrent:process_downloaded_block(ParsedPayload, Data)
                ),
                ?assertEqual(
                    2,
                    meck:num_calls(gen_bittorrent_cb, block_downloaded, ['_', '_', '_', '_', '_'])
                )
            end
        }]
    }.


handle_payload_test_() ->
    OldCbState = {old_cb_state},
    NewCbState = {new_cb_state},
    PieceId = 0,
    ParsedPayload = [
        {piece, #piece_data{
            piece_index  = 0,
            payload      = <<0,1,2,3>>,
            block_offset = <<0,0,0,0>>,
            length       = <<0,0,0,4>>
        }},
        {piece, #piece_data{
            piece_index  = 0,
            payload      = <<4,5,6,7>>,
            block_offset = <<0,0,0,4>>,
            length       = <<0,0,0,4>>
        }}
    ],
    Data = #data{
        blocks                  = [0, 1, 2, 3],
        blocks_not_requested    = [0, 1, 2, 3],
        request_length          = 4,
        cb_mod                  = gen_bittorrent_cb,
        cb_state                = OldCbState,
        piece_id                = PieceId,
        socket                  = socket,
        rest_payload            = <<>>
    },
    State0 = #state{handshake = not_handshaked, leecher_state = not_interested, peer_state = choked},
    State1 = #state{handshake = handshaked, leecher_state = interested, peer_state = choked},
    State2 = #state{handshake = handshaked, leecher_state = interested, peer_state = unchoked},
    {setup,
        fun() ->
            ok = meck:new(gen_bittorrent_cb, [non_strict]),
            ok = meck:new([gen_bittorrent_message, gen_bittorrent_helper, gen_bittorrent_packet], [passthrough]),
            ok = meck:expect(gen_bittorrent_cb, peer_handshaked, fun(_, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_cb, peer_unchoked, fun(_, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_cb, peer_choked, fun(_, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_cb, block_requested, fun(_, _, _, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_cb, block_downloaded, fun(_, _, _, _, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_cb, piece_completed, fun(_, _) -> {ok, NewCbState} end),
            ok = meck:expect(gen_bittorrent_message, interested, ['_'], ok),
            ok = meck:expect(gen_bittorrent_message, request_piece, ['_', '_'], ok),
            ok = meck:expect(gen_bittorrent_helper, get_packet, ['_'], ok),
            ok = meck:expect(gen_bittorrent_packet, parse, fun
                (<<"packet1">>, _) -> {ok, [{handshake, true}], <<>>};
                (<<"packet2">>, _) -> {ok, [{unchoke, true}], <<>>};
                (<<"packet3">>, _) -> {ok, ParsedPayload, <<>>};
                (<<"packet4">>, _) -> {ok, [{choke, true}], <<>>}
            end)
        end,
        fun(_) ->
            true = meck:validate([gen_bittorrent_cb, gen_bittorrent_message, gen_bittorrent_helper, gen_bittorrent_packet]),
            ok = meck:unload([gen_bittorrent_cb, gen_bittorrent_message, gen_bittorrent_helper, gen_bittorrent_packet])
        end,
        [{"Handle first state. Not handshaked, not interested, choked.",
            fun() ->
                ?assertEqual(
                    {next_state, State1, Data#data{cb_state = NewCbState}},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet1">>}, State0, Data)
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_cb, peer_handshaked, ['_', '_'])
                ),
                ?assertEqual(
                    0,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_packet, parse, ['_', '_'])
                )
            end
        },
        {"Handle second state. Handshaked, interested, choked.",
            fun() ->
                ?assertEqual(
                    {next_state, State2, Data#data{cb_state = NewCbState, blocks_not_requested = []}},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet2">>}, State1, Data)
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_cb, peer_unchoked, ['_', '_'])
                ),
                ?assertEqual(
                    4,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                ),
                ?assertEqual(
                    2,
                    meck:num_calls(gen_bittorrent_packet, parse, ['_', '_'])
                )
            end
        },
        {"Handle third state. Handshaked, interested, unchoked. Download block.",
            fun() ->
                ?assertEqual(
                    {next_state, State2, Data#data{cb_state = NewCbState, blocks_not_requested = [], blocks = [2, 3]}},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet3">>}, State2, Data)
                ),
                ?assertEqual(
                    0,
                    meck:num_calls(gen_bittorrent_cb, peer_choked, ['_', '_'])
                ),
                ?assertEqual(
                    2,
                    meck:num_calls(gen_bittorrent_cb, block_downloaded, [PieceId, '_', '_', '_', '_'])
                ),
                ?assertEqual(
                    8,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                ),
                ?assertEqual(
                    3,
                    meck:num_calls(gen_bittorrent_packet, parse, ['_', '_'])
                )
            end
        },
        {"Handle third state. Handshaked, interested, unchoked. Get choke.",
            fun() ->
                ?assertEqual(
                    {next_state, State1, Data#data{cb_state = NewCbState, blocks_not_requested = []}},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet4">>}, State2, Data)
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_cb, peer_choked, ['_', '_'])
                ),
                ?assertEqual(
                    2,
                    meck:num_calls(gen_bittorrent_cb, block_downloaded, [PieceId, '_', '_', '_', '_'])
                ),
                ?assertEqual(
                    12,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                ),
                ?assertEqual(
                    4,
                    meck:num_calls(gen_bittorrent_packet, parse, ['_', '_'])
                )
            end
        },
        {"Handle third state. Handshaked, interested, unchoked. Complete piece.",
            fun() ->
                ?assertEqual(
                    {next_state, State2, Data#data{cb_state = NewCbState, blocks_not_requested = [], blocks = []}, 3000},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet3">>}, State2, Data#data{blocks_not_requested = [], blocks = [0, 1]})
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_cb, peer_choked, ['_', '_'])
                ),
                ?assertEqual(
                    4,
                    meck:num_calls(gen_bittorrent_cb, block_downloaded, [PieceId, '_', '_', '_', '_'])
                ),
                ?assertEqual(
                    12,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(gen_bittorrent_cb, piece_completed, ['_', '_'])
                ),
                ?assertEqual(
                    5,
                    meck:num_calls(gen_bittorrent_packet, parse, ['_', '_'])
                )
            end
        },
        {"Handle third state. Handshaked, interested, unchoked. Get choke. Don't request if all pieces are requested.",
            fun() ->
                ?assertEqual(
                    {next_state, State1, Data#data{cb_state = NewCbState, blocks_not_requested = []}},
                    gen_bittorrent:handle_event(info, {tcp, socket, <<"packet4">>}, State2, Data#data{blocks_not_requested = []})
                ),
                ?assertEqual(
                    12,
                    meck:num_calls(gen_bittorrent_cb, block_requested, [PieceId, '_', '_', '_'])
                )
            end
        }]
    }.


