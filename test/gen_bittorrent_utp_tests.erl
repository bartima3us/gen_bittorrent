%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2020, sarunas.bartusevicius@gmail.com
%%% @doc
%%% gen_bittorrent_utp tests.
%%% @end
%%% Created : 24. Oct 2020 19.55
%%%-------------------------------------------------------------------
-module(gen_bittorrent_utp_tests).
-author("bartimaeus").

-include("gen_bittorrent.hrl").
-include("gen_bittorrent_internal.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Mirrored internal testing process states and state data.
%%%===================================================================

-record(state, {
    active          = false     :: false | once,
    type                        :: sender | receiver,
    parent                      :: pid(),   % Start of this process
    ip                          :: inet:ip_address(),
    port                        :: inet:port_number(),
    socket                      :: port(),
    payload         = <<>>      :: binary(),
    conn_id_recv                :: binary(), % rand()
    conn_id_send                :: binary(), % conn_id_recv + 1
    wnd_size                    :: binary(),
    seq_nr                      :: binary(),
    last_ack_nr                 :: binary(),
    not_acked       = []        :: [binary()], % Sequence numbers to which are still not asked
    peer_not_acked  = []        :: [binary()]  % Sequence numbers to which peer is still not asked
}).


%%%===================================================================
%%% Tests.
%%%===================================================================

%%
%%
%%
do_error_response_test_() ->
    {setup,
        fun() ->
            ok = meck:new(gen_bittorrent_helper),
            ok = meck:expect(gen_bittorrent_helper, get_timestamp_microseconds, [], <<45,31,111,7>>)
        end,
        fun(_) ->
            true = meck:validate(gen_bittorrent_helper),
            ok = meck:unload(gen_bittorrent_helper)
        end,
        [{"SYN packet.",
            fun() ->
                State = #state{
                    conn_id_send = <<0,0,0,1>>,
                    seq_nr       = <<123,11,0,0>>
                },
                ?assertEqual(
                    <<4,
                    1,
                    0,0,
                    0,0,0,1,
                    45,31,111,7,
                    0,0,0,0,0,0,0,0,
                    123,11,0,0,
                    0,0,0,0>>,
                    gen_bittorrent_utp:st_syn(State)
                )
            end
        }]
    }.

