%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2020, sarunas.bartusevicius@gmail.com
%%% @doc
%%% uTP CT testing.
%%% @end
%%% Created : 10. Oct 2020 19.20
%%%-------------------------------------------------------------------
-module(gen_bittorrent_utp_SUITE).
-author("bartimaeus").

-include_lib("eunit/include/eunit.hrl").

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
test_basic(_Config) ->
    ok = meck:new(gen_bittorrent_utp, [passthrough]),
    {ok, ServerPid} = gen_bittorrent_utp:start_link({127,0,0,1}, 7001, self(), 7002),
    % Stop test
    ok = gen_bittorrent:stop(ServerPid),
    true = meck:validate(gen_bittorrent_utp),
    ok = meck:unload(gen_bittorrent_utp),
    ok.


