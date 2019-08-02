%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2018, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Generic gen_bittorrent behaviour implemented as OTP compliant special process FSM.
%%% The purpose of this behaviour is to download a piece.
%%% It can be used as a high level transport protocol for any software.
%%% To avoid any unnecessary overhead, no generic behaviour was used.
%%% gen_bittorrent relies only on proc_lib, sys, gen modules.
%%% Implementation example can be found: https://github.com/bartima3us/erl-bittorrent
%%% @end
%%% Created : 21. Jul 2019 18.12
%%%-------------------------------------------------------------------
-module(gen_bittorrent).
-author("bartimaeus").

-include("gen_bittorrent.hrl").
-include("gen_bittorrent_internal.hrl").

%% API
-export([
    start/9,
    start/10,
    start_link/9,
    start_link/10,
    call/2,
    call/3,
    switch_piece/3,
    stop/1,
    stop/3
]).

-export([
    init_it/6,
    system_continue/3,
    system_terminate/4,
    system_get_state/1,
    system_replace_state/2,
    system_code_change/4,
    wake_hibernate/2
]).

-ifdef(TEST).
-export([
    request_piece/1,
    get_request_data/3,
    get_peer_new_state/2,
    process_downloaded_block/2,
    handle_payload/3
]).
-endif.


%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%
%%
-callback init(
        Args :: list()
    ) ->
        {ok, State :: term(), Timeout :: timeout() | hibernate} | % @todo implement
        {ok, State :: term()}.


%%
%%
%%
-callback peer_handshaked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback peer_unchoked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback peer_choked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback block_requested(
        PieceId :: piece_id_int(),
        Offset  :: binary(),
        Length  :: binary(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback block_downloaded(
        PieceId :: piece_id_int(),
        Payload :: payload(),
        Offset  :: binary(),
        Length  :: binary(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback piece_completed(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback handle_call(
        Request :: term(),
        From    :: {pid(), Tag :: term()},
        State   :: term()
    ) ->
        {reply, Reply :: term(), NewState :: term()} |
        {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
        {stop, Reason :: term(), Reply :: term()}.


%%
%%
%%
-callback handle_info(
        Info :: timeout | term(),
        State :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%
%%
-callback code_change(
        OldVsn :: (term() | {down, term()}),
        State  :: term(),
        Extra  :: term()
    ) ->
        {ok, NewState :: term()} |
        {error, Reason :: term()}.


%%
%%
%%
-callback terminate(
        State  :: term()
    ) ->
        ok.



%%%===================================================================
%%% API.
%%%===================================================================

%%
%%
%%
start(CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen:start(?MODULE, nolink, CbMod, [PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).

start(Name, CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen:start(?MODULE, nolink, Name, CbMod, [PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).

start_link(CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen:start(?MODULE, link, CbMod, [PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).

start_link(Name, CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen:start(?MODULE, link, Name, CbMod, [PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).


%%
%%
%%
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
        {ok, Res}        -> Res;
        {'EXIT', Reason} -> exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
        {ok, Res}        -> Res;
        {'EXIT', Reason} -> exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.


%%  @doc
%%  Change downloading piece to the new one.
%%
switch_piece(Name, PieceId, PieceSize) ->
    Name ! {'$switch_piece', PieceId, PieceSize},
    ok.


%%  @doc
%%  Stop BitTorrent process.
%%
stop(Name) ->
    gen:stop(Name).

stop(Name, Reason, Timeout) ->
    gen:stop(Name, Reason, Timeout).



%%%===================================================================
%%% Callback functions for proc_lib.
%%%===================================================================

init_it(Starter, self, ServerRef, CbMod, Args, Opts) ->
    init_it(Starter, self(), ServerRef, CbMod, Args, Opts);

init_it(Starter, Parent, Name0, CbMod, [PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Opts) ->
    Name = gen:name(Name0),
    Deb = gen:debug_options(Name, Opts),
    HibernateTimeout = gen:hibernate_after(Opts),
    RequestLength    = get_param(request_length, Opts), % @todo validate, must be power of 2
    ConnectTimeout   = get_param(connect_timeout, Opts),
    Protocol         = get_param(protocol, Opts),
    LastBlockId      = trunc(math:ceil(PieceSize / RequestLength)),
    Blocks           = lists:seq(0, LastBlockId - 1),
    proc_lib:init_ack(Starter, {ok, self()}),
    case catch CbMod:init(Args) of
        {ok, CbState} ->
            case do_connect(PeerIp, PeerPort, ConnectTimeout) of
                {ok, Socket} ->
                    ok = gen_bittorrent_message:handshake(Socket, PeerId, TorrentHash),
                    ok = gen_bittorrent_helper:get_packet(Socket),
                    Data = #data{
                        name                    = Name,
                        socket                  = Socket,
                        torrent_hash            = TorrentHash,
                        peer_ip                 = PeerIp,
                        peer_port               = PeerPort,
                        piece_id                = PieceId,
                        piece_size              = PieceSize,
                        cb_mod                  = CbMod,
                        cb_state                = CbState,
                        connect_timeout         = ConnectTimeout,
                        blocks                  = Blocks,
                        blocks_not_requested    = Blocks,
                        protocol                = Protocol,
                        request_length          = RequestLength,
                        peer_id                 = PeerId,
                        args                    = Args,
                        parent                  = Parent,
                        debug                   = Deb,
                        hibernate_timeout       = HibernateTimeout
                    },
                    State = #state{},
                    loop(State, Data);
                {error, Reason} ->
                    terminate(#data{name = Name, debug = Deb}, Reason)
            end;
        {'EXIT', Reason} ->
            proc_lib:init_ack(Starter, {error, Reason}),
            terminate(#data{name = Name, debug = Deb}, {Reason, {CbMod, init, [Args]}});
        Else ->
            proc_lib:init_ack(Starter, {bad_init_return, Else}),
            terminate(#data{name = Name, debug = Deb}, {bad_init_return, Else})
    end.



%%%===================================================================
%%% Callback functions for system messages handling.
%%%===================================================================

%%
%%
%%
system_continue(Parent, Deb, {State, Data}) ->
    loop(State, Data#data{parent = Parent, debug = Deb}).


%%
%%
%%
system_terminate(Reason, _Parent, _Deb, _SD) ->
    exit(Reason).   % @todo fix?


%%
%%
%%
system_get_state({_State, #data{cb_state = CbState}}) ->
    {ok, CbState}.


%%
%%
%%
system_replace_state(StateFun, {State, Data = #data{cb_state = CbState}}) ->
    NewCbState = StateFun(CbState),
    {ok, NewCbState, {State, Data#data{cb_state = NewCbState}}}.


%%
%%
%%
system_code_change({State, Data = #data{cb_mod = CbMod, cb_state = CbState}}, _Module, OldVsn, Extra) ->
    case catch CbMod:code_change(OldVsn, CbState, Extra) of
        {ok, NewState} -> {ok, {State, Data#data{cb_state = NewState}}};
        Other          -> Other
    end.


%%
%%
%%
wake_hibernate(State, Data) ->
    Msg1 = receive
        Msg0 -> Msg0
    end,
    decode_message(Msg1, State, Data#data{timeout = infinity}, true).



%%%===================================================================
%%% Internal generic functions.
%%%===================================================================

%%
%%
%%
loop(State, Data = #data{timeout = hibernate}) ->
    proc_lib:hibernate(?MODULE, wake_hibernate, [State, Data]);

loop(State, Data = #data{timeout = infinity, hibernate_timeout = HibernateTimeout}) ->
    receive
        Msg -> decode_message(Msg, State, Data#data{timeout = infinity}, false)
    after HibernateTimeout ->
        loop(State, Data = #data{timeout = hibernate})
    end;

loop(State, Data = #data{timeout = Timeout}) ->
    Msg1 = receive
        Msg0 -> Msg0
    after Timeout ->
        timeout
    end,
    decode_message(Msg1, State, Data#data{timeout = infinity}, false).


%%
%%
%%
decode_message(Msg, State, Data, Hibernation) ->
    #data{
        socket          = Socket,
        rest_payload    = CurrRestPayload,
        cb_mod          = CbMod,
        cb_state        = CbState,
        name            = Name,
        parent          = Parent,
        debug           = Deb,
        request_length  = RequestLength,
        piece_id        = PieceId
    } = Data,
    case Msg of
        {tcp, Port, Packet} when Port =:= Socket ->
            Deb2 = sys:handle_debug(Deb, fun format_event/3, ?MODULE, {in, {tcp, Port}}),
            {ok, ParsedPayload, NewRestPayload} = gen_bittorrent_packet:parse(Packet, CurrRestPayload),
            case handle_payload(State, Data#data{rest_payload = NewRestPayload}, ParsedPayload) of
                {ok, NewState, NewData} ->
                    ok = gen_bittorrent_helper:get_packet(Socket),
                    loop(NewState, NewData);
                {completed, NewState, NewData0} ->
                    Deb3 = sys:handle_debug(Deb2, fun format_event/3, ?MODULE, completed),
                    case catch CbMod:piece_completed(PieceId, CbState) of
                        {ok, NewCbState} ->
                            loop(NewState, NewData0#data{cb_state = NewCbState, debug = Deb3});
                        {'EXIT', Reason} ->
                            terminate(NewData0#data{debug = Deb3}, Reason);
                        {stop, Reason} ->
                            terminate(NewData0#data{debug = Deb3}, Reason)
                    end
            end;
        {tcp_closed, Port} when Port =:= Socket ->
            terminate(Data, {socket_error, tcp_closed});
        {'$gen_call', From, CallMsg} ->
            Deb2 = sys:handle_debug(Deb, fun format_event/3, ?MODULE, {in, {'$gen_call', From, Msg}}),
            case catch CbMod:handle_call(CallMsg, From, CbState) of
                {reply, Reply, NewCbState} ->
                    Deb3 = reply(From, Reply, CbMod, CbState, Name, Deb2),
                    loop(State, Data#data{cb_state = NewCbState, debug = Deb3});
                {reply, Reply, NewCbState, NewTimeout} ->
                    Deb3 = reply(From, Reply, CbMod, CbState, Name, Deb2),
                    loop(State, Data#data{cb_state = NewCbState, debug = Deb3, timeout = NewTimeout});
                {stop, Reason, Reply} ->
                    reply(From, Reply, CbMod, CbState, Name, Deb),
                    terminate(Data#data{debug = Deb2}, Reason);
                {'EXIT', Reason} ->
                    terminate(Data#data{debug = Deb2}, Reason)
            end;
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, {State, Data}, Hibernation);
        {'$switch_piece', NewPieceId, NewPieceSize} ->
            LastBlockId = trunc(math:ceil(NewPieceSize / RequestLength)),
            Blocks      = lists:seq(0, LastBlockId - 1),
            NewData0 = Data#data{
                piece_id             = NewPieceId,
                piece_size           = NewPieceSize,
                blocks               = Blocks,
                blocks_not_requested = Blocks
            },
            NewData1 = request_piece(NewData0),
            ok = gen_bittorrent_helper:get_packet(Socket),
            loop(State, NewData1);
        Other ->
            case catch CbMod:handle_info(Other, CbState) of
                {ok, NewCbState} ->
                    Deb2 = sys:handle_debug(Deb, fun format_event/3, ?MODULE, {in, {msg, Other}}),
                    loop(State, Data#data{cb_state = NewCbState, debug = Deb2});
                {stop, Reason} ->
                    terminate(Data, Reason);
                {'EXIT', Reason} ->
                    terminate(Data, Reason)
            end
    end.


%%
%%
%%
terminate(Data, Reason) ->
    #data{
        name     = Name,
        debug    = Debug,
        cb_state = CbState,
        cb_mod   = CbMod
    } = Data,
    terminate(Name, Debug, CbMod, CbState, Reason).

terminate(Name, Debug, CbMod, CbState, Reason) when CbMod =/= undefined ->
    _ = CbMod:terminate(CbState),
    terminate(Name, Debug, Reason);

terminate(Name, Debug, _CbMod, _CbState, Reason) ->
    terminate(Name, Debug, Reason).

terminate(Name, Debug, Reason) ->
    gen:unregister_name(Name),
    sys:print_log(Debug),
    exit(Reason).


%%
%%
%%
reply({To, Tag}, Reply, CbMod, CbState, Name, Debug0)  ->
    Debug1 = sys:handle_debug(Debug0, fun format_event/3, self(), {out, Reply, To, CbState}),
    case catch To ! {Tag, Reply} of
        {'EXIT', Reason} -> terminate(Name, Debug1, CbMod, CbState, Reason);
        _Other           -> Debug1
    end.


%%
%%
%%
format_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg ~p~n", [Name, Event]).



%%%===================================================================
%%% Internal BitTorrent functions.
%%%===================================================================

%%  @private
%%  Open socket with a peer.
%%
do_connect(PeerIp, Port, ConnectTimeout) ->
    case gen_tcp:connect(PeerIp, Port, [{active, false}, binary], ConnectTimeout) of
        {error, emfile} ->
            error_logger:warning_msg("File descriptor is exhausted. Can't open new a socket for leecher."),
            {error, emfile};
        {error, Error} ->
            % @todo what todo on timeout? Especially if it becomes constantly? Maybe freeze such peer
            {error, {socket_error, Error}};
        Result ->
            Result
    end.


%%  @private
%%  Make a pipelined request of the piece.
%%
request_piece(Data) ->
    #data{
        socket                  = Socket,
        piece_id                = PieceId,
        piece_size              = PieceSize,
        blocks_not_requested    = BlocksNotRequested,
        request_length          = RequestLength,
        cb_mod                  = CbMod
    } = Data,
    %
    % Maybe PieceData process downloaded all pieces?
    case BlocksNotRequested of
        [_|_] -> % @todo make requesting in chunks?
            % If not, make requests for blocks
            {RequestMessage, NewData} = lists:foldl(
                fun (NextBlockId, {MsgAcc, DataAcc}) ->
                    #data{
                        cb_state             = CbStateAcc,
                        blocks_not_requested = BlocksNotRequestedAcc
                    } = DataAcc,
                    {ok, {OffsetBin, NextLength}} = get_request_data(NextBlockId, PieceSize, RequestLength),
                    PieceIdBin = gen_bittorrent_helper:int_piece_id_to_bin(PieceId),
                    PieceSizeBin = <<NextLength:32>>,
                    Message = gen_bittorrent_message:create_pipeline_request_piece(MsgAcc, PieceIdBin, OffsetBin, PieceSizeBin),
                    NewCbStateAcc = case catch CbMod:block_requested(PieceId, OffsetBin, NextLength, CbStateAcc) of
                        {ok, NewCbState} -> NewCbState;
                        {stop, Reason}   -> terminate(DataAcc, Reason);
                        {'EXIT', Reason} -> terminate(DataAcc, Reason)
                    end,
                    {Message, DataAcc#data{cb_state = NewCbStateAcc, blocks_not_requested = BlocksNotRequestedAcc -- [NextBlockId]}}
                end,
                {<<"">>, Data},
                BlocksNotRequested
            ),
            gen_bittorrent_message:request_piece(Socket, RequestMessage),
            NewData;
        [] ->
            Data
    end.


%%  @private
%%  Get increased `length` and `offset`.
%%
get_request_data(BlockId, PieceSize, RequestLength) ->
    OffsetBin = <<(RequestLength * BlockId):32>>,
    <<OffsetInt:32>> = OffsetBin,
    % Last chunk of piece would be shorter than default length so we need to check if next chunk isn't a last
    NextLength = case (OffsetInt + RequestLength) =< PieceSize of
        true  -> RequestLength;
        false -> PieceSize - OffsetInt
    end,
    {ok, {OffsetBin, NextLength}}.


%%  @private
%%  Get new state of peer.
%%
get_peer_new_state(ParsedPayload, DefaultState) ->
    lists:foldl(
        fun
            ({unchoke, true}, _Acc) -> unchoked;
            ({choke, true},   _Acc) -> choked;
            (_,                Acc) -> Acc
        end,
        DefaultState,
        ParsedPayload
    ).


%%  @private
%%  Get parameter from options.
%%
get_param(request_length, Opts)  -> proplists:get_value(request_length, Opts, ?DEFAULT_REQUEST_LENGTH);
get_param(protocol, Opts)        -> proplists:get_value(protocol, Opts, ?DEFAULT_PROTOCOL);
get_param(connect_timeout, Opts) -> proplists:get_value(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT).


%%  @private
%%  Process downloaded block.
%%
process_downloaded_block(ParsedPayload, Data) ->
    #data{
        cb_state        = CbState0,
        blocks          = Blocks,
        debug           = Debug,
        request_length  = RequestLength,
        cb_mod          = CbMod
    } = Data,
    {NewCbState, NewBlocks, NewDebug} = lists:foldl(
        fun
            ({piece, PD = #piece_data{}}, Acc = {AccCbState, AccBlocksLeft, AccDebug}) ->
                #piece_data{
                    payload      = Payload,
                    block_offset = Offset,
                    length       = Length,
                    piece_index  = PieceId
                } = PD,
                <<BlockBegin:32>> = Offset,
                BlockId = trunc(BlockBegin / RequestLength),
                case lists:member(BlockId, Blocks) of
                    true  ->
                        NewAccBlocksLeft = AccBlocksLeft -- [BlockId],
                        case catch CbMod:block_downloaded(PieceId, Payload, Offset, Length, AccCbState) of
                            {ok, NewCbState1} ->
                                NewAccDebug = sys:handle_debug(AccDebug, fun format_event/3, ?MODULE, {in, block, BlockId}),
                                {NewCbState1, NewAccBlocksLeft, NewAccDebug};
                            {stop, Reason} ->
                                terminate(Data#data{cb_state = AccCbState, debug = AccDebug}, Reason);
                            {'EXIT', Reason} ->
                                terminate(Data#data{cb_state = AccCbState, debug = AccDebug}, Reason)
                        end;
                    false ->
                        Acc
                end;
           (_Else, Acc) ->
               Acc
        end,
        {CbState0, Blocks, Debug},
        ParsedPayload
    ),
    Data#data{
        cb_state = NewCbState,
        blocks   = NewBlocks,
        debug    = NewDebug
    }.


%%  @private
%%  First state. Trying to handshake.
%%
handle_payload(State = #state{handshake = not_handshaked, leecher_state = not_interested, peer_state = choked}, Data, ParsedPayload) ->
    #data{
        socket    = Socket,
        cb_mod    = CbMod,
        cb_state  = CbState,
        debug     = Debug0,
        piece_id  = PieceId
    } = Data,
    {NewCbState, Debug1} = case proplists:get_value(handshake, ParsedPayload) of
        true ->
            case catch CbMod:peer_handshaked(PieceId, CbState) of
                {ok, NewCbState0} ->
                    gen_bittorrent_message:interested(Socket),
                    Deb = sys:handle_debug(Debug0, fun format_event/3, ?MODULE, {in, handshake}),
                    {NewCbState0, Deb};
                {stop, Reason} ->
                    terminate(Data, Reason);
                {'EXIT', Reason} ->
                    terminate(Data, Reason)
            end;
        _ ->
            {CbState, Debug0}
    end,
    {ok, State#state{handshake = handshaked, leecher_state = interested}, Data#data{cb_state = NewCbState, debug = Debug1}};

%%  @private
%%  Second state. Enter interested mode. Trying to unchoke peer.
%%
handle_payload(State = #state{handshake = handshaked, leecher_state = interested, peer_state = choked}, Data, ParsedPayload) ->
    #data{
        socket    = Socket,
        cb_mod    = CbMod,
        debug     = Debug0,
        piece_id  = PieceId
    } = Data,
    NewData0 = #data{cb_state = NewCbState0} = request_piece(Data),
    ok = gen_bittorrent_helper:get_packet(Socket),
    {NewPeerState, NewCbState3, Debug1} = case get_peer_new_state(ParsedPayload, choked) of
        unchoked ->
            NewCbState2 = case catch CbMod:peer_unchoked(PieceId, NewCbState0) of
                {ok, NewCbState1} -> NewCbState1;
                {stop, Reason}    -> terminate(NewData0, Reason);
                {'EXIT', Reason}  -> terminate(NewData0, Reason)
            end,
            Deb = sys:handle_debug(Debug0, fun format_event/3, ?MODULE, {in, unchoked}),
            {unchoked, NewCbState2, Deb};
        choked ->
            {choked, NewCbState0, Debug0}
    end,
    {ok, State#state{peer_state = NewPeerState}, NewData0#data{cb_state = NewCbState3, debug = Debug1}};

%%  @private
%%  Third state. Retrieve blocks from peer.
%%
handle_payload(State = #state{handshake = handshaked, leecher_state = interested, peer_state = unchoked}, Data, ParsedPayload) ->
    #data{
        cb_mod   = CbMod,
        debug    = Debug0,
        piece_id = PieceId,
        blocks   = Blocks
    } = Data,
    NewData0 = #data{cb_state = NewCbState0} = request_piece(Data),
    NewData1 = process_downloaded_block(ParsedPayload, Data#data{cb_state = NewCbState0, debug = Debug0}),
    #data{cb_state = NewCbState1, blocks = NewBlocks0, debug = Debug1} = NewData1,
    {NewPeerState, NewData2} = case get_peer_new_state(ParsedPayload, unchoked) of
        unchoked ->
            Debug2 = sys:handle_debug(Debug1, fun format_event/3, ?MODULE, {in, unchoked}),
            {unchoked, NewData0#data{
                blocks   = NewBlocks0,
                cb_state = NewCbState1,
                debug    = Debug2
            }};
        choked ->
            NewCbState3 = case catch CbMod:peer_choked(PieceId, NewCbState1) of
                {ok, NewCbState2} -> NewCbState2;
                {stop, Reason}    -> terminate(NewData0, Reason);
                {'EXIT', Reason}  -> terminate(NewData0, Reason)
            end,
            Debug2 = sys:handle_debug(Debug1, fun format_event/3, ?MODULE, {in, choked}),
            {choked, NewData0#data{
                blocks   = Blocks,
                cb_state = NewCbState3,
                debug    = Debug2
            }}
    end,
    #data{blocks = NewBlocks} = NewData2,
    case NewBlocks of
        [_|_] -> {ok, State#state{handshake = handshaked, leecher_state = interested, peer_state = NewPeerState}, NewData2};
        []    -> {completed, State, NewData2}
    end.


