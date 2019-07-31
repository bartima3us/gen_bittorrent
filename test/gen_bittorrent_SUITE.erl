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

-include_lib("eunit/include/eunit.hrl").

-define(MECK_TIMEOUT, 5000).
-define(IP, {127,0,0,1}).
-define(PORT, 8092).
-define(FILENAME, "file_to_download.example").
-define(STD_PIECE_SIZE, 65536).
-define(PIECE1_SIZE, 65536).
-define(PIECE2_SIZE, 36864).
-define(TORRENT_HASH, "torr3nt_ha5h00000000").
-define(LEECHER_ID, "l33ch3r_1d0000000000").
-define(PEER_ID, "p33r_1d0000000000000").
-define(PIECE_ID1, 0).
-define(PIECE_ID2, 1).

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
%%  Download full file test.
%%  Start -> download piece1 -> switch -> download piece2 -> stop.
%%
test_basic(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    FileSrc = filename:join([DataDir, ?FILENAME]),
    FileDst = filename:join([PrivDir, ?FILENAME]),
    ok = meck:new(gen_bittorrent_event_handler, [passthrough]),
    {ok, ServerPid} = gen_bittorrent_server_mock:start_link(?PORT, ?PEER_ID, ?TORRENT_HASH, FileSrc, ?STD_PIECE_SIZE),
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [server_started, '_'], ?MECK_TIMEOUT),
    {ok, LeecherPid} = gen_bittorrent_impl:start_link(FileDst, ?IP, ?PORT, ?LEECHER_ID, ?PIECE_ID1, ?PIECE1_SIZE),
    % Check for a handshake
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{handshaked, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{handshaked, ?PIECE_ID1}, '_']),
    % Check for unchoke
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{unchoked, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{unchoked, ?PIECE_ID1}, '_']),
    % Check for block request
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    4 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID1}, '_']),
    % Check for block downloaded
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{block_downloaded, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    % Check for completed
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{completed, ?PIECE_ID1}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{completed, ?PIECE_ID1}, '_']),
    4 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{block_downloaded, ?PIECE_ID1}, '_']),
    % --------- Switch piece ---------
    ok = gen_bittorrent:switch_piece(LeecherPid, ?PIECE_ID2, ?PIECE2_SIZE),
    ok = meck:reset(gen_bittorrent_event_handler),
    % Check for a handshake
    0 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{handshaked, ?PIECE_ID2}, '_']),
    % Check for unchoke
    0 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{unchoked, ?PIECE_ID2}, '_']),
    % Check for block request
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID2}, '_'], ?MECK_TIMEOUT),
    3 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{block_requested, ?PIECE_ID2}, '_']),
    % Check for block downloaded
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{block_downloaded, ?PIECE_ID2}, '_'], ?MECK_TIMEOUT),
    % Check for completed
    ok = meck:wait(gen_bittorrent_event_handler, handle_event, [{completed, ?PIECE_ID2}, '_'], ?MECK_TIMEOUT),
    1 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{completed, ?PIECE_ID2}, '_']),
    3 = meck:num_calls(gen_bittorrent_event_handler, handle_event, [{block_downloaded, ?PIECE_ID2}, '_']),
    % Compare files
    {ok, SeederFile} = file:read_file(FileSrc),
    {ok, LeecherFile} = file:read_file(FileDst),
    ?assertEqual(crypto:hash(sha, SeederFile), crypto:hash(sha, LeecherFile)),
    % Stop test
    ok = gen_bittorrent:stop(LeecherPid),
    ok = gen_server:stop(ServerPid),
    true = meck:validate(gen_bittorrent_event_handler),
    ok = meck:unload(gen_bittorrent_event_handler),
    ok.


