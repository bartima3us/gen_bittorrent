%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2018, sarunas.bartusevicius@gmail.com
%%% @doc
%%% gen_bittorrent test implementation.
%%% @end
%%% Created : 30. Jul 2019 22.25
%%%-------------------------------------------------------------------
-module(gen_bittorrent_impl).
-author("bartimaeus").

-behaviour(gen_bittorrent).

%% API
-export([
    start_link/4
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

-record(state, {
    event_handler   :: pid()
}).

%%
%%
%%
start_link(Ip, Port, LeecherId, PieceId) ->
    gen_bittorrent:start_link(?MODULE, Ip, Port, LeecherId, <<20,6,22,150,209,14,72,58,240,183,227,28,144,88,78,197,85,137,236,91>>, PieceId, 1048576, [], []).

%%
%%
%%
init(_Args) ->
    {ok, EventMgrPid} = gen_event:start_link(),
    gen_event:add_handler(EventMgrPid, gen_bittorrent_event_handler, []),
    {ok, #state{event_handler = EventMgrPid}}.

peer_handshaked(PieceId, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {handshaked, PieceId}),
    {ok, State}.

peer_unchoked(PieceId, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {unchoked, PieceId}),
    {ok, State}.

peer_choked(_PieceId, State) ->
    {ok, State}.

block_requested(PieceId, _Offset, _Length, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {block_requested, PieceId}),
    {ok, State}.

block_downloaded(_PieceId, _Payload, Offset, Length, State) ->
    ct:print("Block downloaded! Offset / length = ~p / ~p~n", [Offset, Length]),
    {ok, State}.

piece_completed(_PieceId, State) ->
    ct:print("Piece completed!~n"),
    {ok, State}.

handle_call(Msg, _From, State) ->
    ct:print("Handle call. Msg = ~p~n", [Msg]),
    {reply, ok, State}.

handle_info(Msg, State) ->
    ct:print("Handle info. Msg = ~p~n", [Msg]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_State) ->
    ok.