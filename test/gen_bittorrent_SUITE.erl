%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Behaviour test.
%%% @end
%%% Created : 30. Jul 2019 21.56
%%%-------------------------------------------------------------------
-module(gen_bittorrent_SUITE).
-author("bartimaeus").

-define(MECK_TIMEOUT, 5000).
-define(PORT, 8092).
-define(TORRENT_HASH, "torr3t_ha5h000000000").
-define(LEECHER_ID, "l33ch3r_1d0000000000").
-define(PEER_ID, "p33r_1d0000000000000").
-define(PIECE_ID1, 0).

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% CT tests.
-export([
    test_basic/1
]).


%%%===================================================================
%%% API for CT.
%%%===================================================================

%%  @doc
%%  List of test cases.
%%
all() ->
    [
        test_basic
    ].


%%  @doc
%%  Init.
%%
init_per_suite(Config) ->
    Config.


%%  @doc
%%  Clear.
%%
end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Testcases.
%%%===================================================================

%%
%%
%%
test_basic(Config) ->
%%    ct:print(proplists:get_value(data_dir, Config)),
    ok = meck:new(gen_bittorrent_event_handler, [passthrough]),
    {ok, _} = gen_bittorrent_server_mock:start_link(?PORT, ?PEER_ID, ?TORRENT_HASH),
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [server_started, '_'], ?MECK_TIMEOUT),
    {ok, ServerPid} = gen_bittorrent_impl:start_link({127,0,0,1}, ?PORT, ?LEECHER_ID, ?PIECE_ID1),
    % Check for a handshake
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{handshaked, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{handshaked, ?PIECE_ID1}, '_']),
    % Check for unchoke
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{unchoked, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{unchoked, ?PIECE_ID1}, '_']),
    % Check for block request
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    64 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID1}, '_']),
    timer:sleep(4000),
    ok = gen_server:stop(ServerPid),
    true = meck:validate(gen_bittorrent_event_handler),
    ok = meck:unload(gen_bittorrent_event_handler),
    ok.



