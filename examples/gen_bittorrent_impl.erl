%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2018, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2019 19.29
%%%-------------------------------------------------------------------
-module(gen_bittorrent_impl).
-author("bartimaeus").

%% API
-export([
    start_link/0
]).

-export([
    init/1,
    peer_handshaked/2,
    peer_unchoked/2,
    peer_choked/2,
    block_requested/4,
    block_downloaded/5,
    piece_completed/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/1
]).


%%
%%
%%
start_link() ->
    gen_bittorrent:start_link(?MODULE, {109,161,87,5}, 26449, "-ER0000-45AF6T-NM81-", <<20,6,22,150,209,14,72,58,240,183,227,28,144,88,78,197,85,137,236,91>>, 3, 1048576, [], []).

%%
%%
%%
init(_Args) ->
    {ok, []}.

peer_handshaked(_PieceId, State) ->
    io:format("Handshaked!~n"),
    {ok, State}.

peer_unchoked(_PieceId, State) ->
    io:format("Unchoked!~n"),
    {ok, State}.

peer_choked(_PieceId, State) ->
    io:format("Choked!~n"),
    {ok, State}.

block_requested(_PieceId, Offset, Length, State) ->
    io:format("Block requested! Offset / length = ~p / ~p~n", [Offset, Length]),
    {ok, State}.

block_downloaded(_PieceId, _Payload, Offset, Length, State) ->
    io:format("Block downloaded! Offset / length = ~p / ~p~n", [Offset, Length]),
    {ok, State}.

piece_completed(_PieceId, State) ->
    io:format("Piece completed!~n"),
    {ok, State}.

handle_call(Msg, _From, State) ->
    io:format("Handle call. Msg = ~p~n", [Msg]),
    {reply, answer, State}.

handle_info(Msg, State) ->
    io:format("Handle info. Msg = ~p~n", [Msg]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_State) ->
    ok.