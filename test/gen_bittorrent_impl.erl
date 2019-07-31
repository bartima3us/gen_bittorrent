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
    start_link/6
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
    piece_size      :: pos_integer(),
    event_handler   :: pid(),
    io_device       % @todo specify type
}).


%%
%%
%%
start_link(File, Ip, Port, LeecherId, PieceId, PieceSize) ->
    gen_bittorrent:start_link(?MODULE, Ip, Port, LeecherId, <<20,6,22,150,209,14,72,58,240,183,227,28,144,88,78,197,85,137,236,91>>, PieceId, PieceSize, [File, PieceSize], []).


%%
%%
%%
init([File, PieceSize]) ->
    {ok, EventMgrPid} = gen_event:start_link(),
    gen_event:add_handler(EventMgrPid, gen_bittorrent_event_handler, []),
    {ok, IoDevice} = file:open(File, [write, read, binary]),
    NewState = #state{
        piece_size    = PieceSize,
        event_handler = EventMgrPid,
        io_device     = IoDevice
    },
    {ok, NewState}.


%%
%%
%%
peer_handshaked(PieceId, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {handshaked, PieceId}),
    {ok, State}.


%%
%%
%%
peer_unchoked(PieceId, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {unchoked, PieceId}),
    {ok, State}.


%%
%%
%%
peer_choked(_PieceId, State) ->
    {ok, State}.


%%
%%
%%
block_requested(PieceId, _Offset, _Length, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {block_requested, PieceId}),
    {ok, State}.


%%
%%
%%
block_downloaded(PieceId, Payload, Offset, _Length, State) ->
    #state{
        event_handler = EventMgrPid,
        io_device     = IoDevice,
        piece_size    = PieceSize
    } = State,
    OffsetInt = gen_bittorrent_helper:bin_piece_id_to_int(Offset),
    ok = write_payload(IoDevice, PieceId, OffsetInt, PieceSize, Payload),
    ok = gen_event:notify(EventMgrPid, {block_downloaded, PieceId}),
    {ok, State}.


%%
%%
%%
piece_completed(PieceId, State = #state{event_handler = EventMgrPid}) ->
    ok = gen_event:notify(EventMgrPid, {completed, PieceId}),
    {ok, State}.


%%
%%
%%
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%%
%%
%%
handle_info(_Msg, State) ->
    {ok, State}.


%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%%
%%
terminate(_State) ->
    ok.



%%%===================================================================
%%% Internal functions.
%%%===================================================================

%%
%%
%%
write_payload(IoDevice, PieceId, BlockBegin, PieceSize, Payload) ->
    SizeFrom = PieceSize * PieceId + BlockBegin,
    file:pwrite(IoDevice, [{{bof, SizeFrom}, Payload}]).

