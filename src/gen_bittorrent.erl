%%%-------------------------------------------------------------------
%%% @author sarunas
%%% @copyright (C) 2020, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Generic gen_bittorrent behaviour implemented as OTP behaviour.
%%% The purpose of this behaviour is to download a piece.
%%% It can be used as a high level transport protocol for any software.
%%% gen_bittorrent relies on gen_statem behaviour.
%%% Implementation example can be found: https://github.com/bartima3us/erl-bittorrent
%%% @end
%%% Created : 29. Feb 2020 14.11
%%%-------------------------------------------------------------------
-module(gen_bittorrent).
-author("bartimaeus").
-behavior(gen_statem).
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

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-ifdef(TEST).
-export([
    request_piece/1,
    get_request_data/3,
    get_peer_new_state/2,
    process_downloaded_block/2
]).
-endif.


%%% ============================================================================
%%% Callback definitions.
%%% ============================================================================

%%
%%  Callback process init function. Call when new process is initiated.
%%
-callback init(
        Args :: list()
    ) ->
        {ok, State :: term(), Timeout :: timeout() | hibernate} |
        {ok, State :: term()}.


%%
%%  Call when handshake is done.
%%
-callback peer_handshaked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%  Call when peer state changes into `unchoke`.
%%
-callback peer_unchoked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%  Call when peer state changes into `choke`.
%%
-callback peer_choked(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%  Call when block is requested.
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
%%  Call when new block is downloaded.
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
%%  Call when piece downloading is completed.
%%
-callback piece_completed(
        PieceId :: piece_id_int(),
        State   :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%  Handle synchronous messages.
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
%%  Handle asynchronous messages.
%%
-callback handle_info(
        Info :: timeout | term(),
        State :: term()
    ) ->
        {ok, NewState :: term()} |
        {stop, Reason :: term()}.


%%
%%  Code change function.
%%
-callback code_change(
        OldVsn :: (term() | {down, term()}),
        State  :: term(),
        Extra  :: term()
    ) ->
        {ok, NewState :: term()} |
        {error, Reason :: term()}.


%%
%%  Call when process will be terminated.
%%
-callback terminate(
        State  :: term()
    ) ->
        ok.



%%%===================================================================
%%% API.
%%%===================================================================

%%  @doc
%%  Starts new process without link and without name.
%%
-spec start(
    CbMod       :: module(),
    PeerIp      :: inet:ip_address(),
    PeerPort    :: inet:port_number(),
    PeerId      :: string(),
    TorrentHash :: binary(),
    PieceId     :: non_neg_integer(),
    PieceSize   :: pos_integer(),
    Args        :: [term()],
    Options     :: [term()]
) ->
    {ok, ProcessPid :: pid()}.

start(CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen_statem:start(?MODULE, [CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).


%%  @doc
%%  Starts new process without link and with name.
%%
-spec start(
    Name        :: term(),
    CbMod       :: module(),
    PeerIp      :: inet:ip_address(),
    PeerPort    :: inet:port_number(),
    PeerId      :: string(),
    TorrentHash :: binary(),
    PieceId     :: non_neg_integer(),
    PieceSize   :: pos_integer(),
    Args        :: [term()],
    Options     :: [term()]
) ->
    {ok, ProcessPid :: pid()}.

start(Name, CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen_statem:start(Name, ?MODULE, [CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).


%%  @doc
%%  Starts new process with link and without name.
%%
-spec start_link(
    CbMod       :: module(),
    PeerIp      :: inet:ip_address(),
    PeerPort    :: inet:port_number(),
    PeerId      :: string(),
    TorrentHash :: binary(),
    PieceId     :: non_neg_integer(),
    PieceSize   :: pos_integer(),
    Args        :: [term()],
    Options     :: [term()]
) ->
    {ok, ProcessPid :: pid()}.

start_link(CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen_statem:start_link(?MODULE, [CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).


%%  @doc
%%  Starts new process with link and with name.
%%
-spec start_link(
    Name        :: term(),
    CbMod       :: module(),
    PeerIp      :: inet:ip_address(),
    PeerPort    :: inet:port_number(),
    PeerId      :: string(),
    TorrentHash :: binary(),
    PieceId     :: non_neg_integer(),
    PieceSize   :: pos_integer(),
    Args        :: [term()],
    Options     :: [term()]
) ->
    {ok, ProcessPid :: pid()}.

start_link(Name, CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args, Options) ->
    gen_statem:start_link(Name, ?MODULE, [CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args], Options).


%%  @doc
%%  Send synchronous message with default timeout (5000 ms).
%%
-spec call(
    Name    :: term(),
    Request :: term()
) ->
    Response :: term().

call(Name, Request) ->
    gen_statem:call(Name, {call, Request}).


%%  @doc
%%  Send synchronous message with timeout.
%%
-spec call(
    Name    :: term(),
    Request :: term(),
    Timeout :: pos_integer()
) ->
    Response :: term().

call(Name, Request, Timeout) ->
    gen_statem:call(Name, {call, Request}, Timeout).


%%  @doc
%%  Change downloading piece to the new one.
%%
-spec switch_piece(
    Name      :: term(),
    PieceId   :: non_neg_integer(),
    PieceSize :: pos_integer()
) ->
    ok.

switch_piece(Name, PieceId, PieceSize) ->
    gen_statem:cast(Name, {switch_piece, PieceId, PieceSize}).


%%  @doc
%%  Stop BitTorrent process without reason.
%%
-spec stop(
    Name :: term()
) ->
    ok.

stop(Name) ->
    gen_statem:stop(Name).


%%  @doc
%%  Stop BitTorrent process with reason and timeout.
%%
-spec stop(
    Name    :: term(),
    Reason  :: term(),
    Timeout :: pos_integer() | infinity
) ->
    ok.

stop(Name, Reason, Timeout) ->
    gen_statem:stop(Name, Reason, Timeout).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%
%%
%%
init([CbMod, PeerIp, PeerPort, PeerId, TorrentHash, PieceId, PieceSize, Args]) ->
    RequestLength    = get_param(request_length, Args), % @todo validate, must be power of 2
    ConnectTimeout   = get_param(connect_timeout, Args),
    Protocol         = get_param(protocol, Args),
    LastBlockId      = trunc(math:ceil(PieceSize / RequestLength)),
    Blocks           = lists:seq(0, LastBlockId - 1),
    DoInitFun = fun (CbState, Actions) ->
        case do_connect(PeerIp, PeerPort, ConnectTimeout) of
            {ok, Socket} ->
                ok = gen_bittorrent_message:handshake(Socket, PeerId, TorrentHash),
                ok = gen_bittorrent_helper:get_packet(Socket),
                Data = #data{
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
                    args                    = Args
                },
                {ok, #state{}, Data, Actions};
            {error, Reason} ->
                stop(Reason)
        end
    end,
    case catch CbMod:init(Args) of
        {ok, CbState} ->
            DoInitFun(CbState, []);
        {ok, CbState, infinity} ->
            DoInitFun(CbState, []);
        {ok, CbState, hibernate} ->
            DoInitFun(CbState, [hibernate]);
        {ok, CbState, Timeout} when is_integer(Timeout) ->
            DoInitFun(CbState, [{{timeout, global}, Timeout, timeout}]);
        {'EXIT', Reason} ->
            stop(Reason);
        Else ->
            stop({bad_init_return, Else})
    end.


%%
%%
%%
callback_mode() ->
    [handle_event_function].



%%%===================================================================
%%% States
%%%===================================================================

%--------------------------------------------------------------------
%   First state. Trying to handshake.
%
handle_event(
    info,
    {tcp, Port, Packet},
    State = #state{handshake = not_handshaked, leecher_state = not_interested, peer_state = choked},
    SD = #data{socket = Socket, cb_mod = CbMod, cb_state = CbState, piece_id = PieceId, rest_payload = CurrRestPayload}
) when Port =:= Socket ->
    {ok, ParsedPayload, NewRestPayload} = gen_bittorrent_packet:parse(Packet, CurrRestPayload),
    NewCbState = case proplists:get_value(handshake, ParsedPayload) of
        true ->
            case catch CbMod:peer_handshaked(PieceId, CbState) of
                {ok, NewCbState0} ->
                    gen_bittorrent_message:interested(Socket),
                    ok = gen_bittorrent_helper:get_packet(Socket),
                    NewCbState0;
                {stop, Reason} ->
                    stop(Reason);
                {'EXIT', Reason} ->
                    stop(Reason)
            end;
        _ ->
            CbState
    end,
    {next_state, State#state{handshake = handshaked, leecher_state = interested}, SD#data{cb_state = NewCbState, rest_payload = NewRestPayload}};

%--------------------------------------------------------------------
%   Second state. Enter interested mode. Trying to unchoke peer.
%
handle_event(
    info,
    {tcp, Port, Packet},
    State = #state{handshake = handshaked, leecher_state = interested, peer_state = choked},
    SD = #data{socket = Socket, cb_mod = CbMod, piece_id = PieceId, rest_payload = CurrRestPayload}
) when Port =:= Socket ->
    SD0 = #data{cb_state = NewCbState0} = request_piece(SD),
    {ok, ParsedPayload, NewRestPayload} = gen_bittorrent_packet:parse(Packet, CurrRestPayload),
    ok = gen_bittorrent_helper:get_packet(Socket),
    {NewPeerState, NewCbState3} = case get_peer_new_state(ParsedPayload, choked) of
        unchoked ->
            NewCbState2 = case catch CbMod:peer_unchoked(PieceId, NewCbState0) of
                {ok, NewCbState1} -> NewCbState1;
                {stop, Reason}    -> stop(Reason);
                {'EXIT', Reason}  -> stop(Reason)
            end,
            {unchoked, NewCbState2};
        choked ->
            {choked, NewCbState0}
    end,
    {next_state, State#state{peer_state = NewPeerState}, SD0#data{cb_state = NewCbState3, rest_payload = NewRestPayload}};

%--------------------------------------------------------------------
%   Third state. Retrieve blocks from peer.
%
handle_event(
    info,
    {tcp, Port, Packet},
    State = #state{handshake = handshaked, leecher_state = interested, peer_state = unchoked},
    SD = #data{socket = Socket, cb_mod = CbMod, cb_state = CbState, piece_id = PieceId, rest_payload = CurrRestPayload, blocks = Blocks}
) when Port =:= Socket ->
    SD0 = #data{cb_state = NewCbState0} = request_piece(SD),
    {ok, ParsedPayload, NewRestPayload} = gen_bittorrent_packet:parse(Packet, CurrRestPayload),
    ok = gen_bittorrent_helper:get_packet(Socket),
    SD1 = process_downloaded_block(ParsedPayload, SD0#data{cb_state = NewCbState0}),
    #data{cb_state = NewCbState1, blocks = NewBlocks0} = SD1,
    {NewPeerState, SD2} = case get_peer_new_state(ParsedPayload, unchoked) of
        unchoked ->
            {unchoked, SD1#data{
                blocks   = NewBlocks0,
                cb_state = NewCbState1
            }};
        choked ->
            NewCbState3 = case catch CbMod:peer_choked(PieceId, NewCbState1) of
                {ok, NewCbState2} -> NewCbState2;
                {stop, Reason0}    -> stop(Reason0);
                {'EXIT', Reason0}  -> stop(Reason0)
            end,
            {choked, SD1#data{
                blocks   = Blocks,
                cb_state = NewCbState3
            }}
    end,
    #data{blocks = NewBlocks} = SD2,
    case NewBlocks of
        [_|_] ->
            {next_state, State#state{peer_state = NewPeerState}, SD2#data{rest_payload = NewRestPayload}};
        []    ->
            case catch CbMod:piece_completed(PieceId, CbState) of
                {ok, NewCbState} ->
                    {next_state, State#state{peer_state = NewPeerState}, SD2#data{cb_state = NewCbState, rest_payload = NewRestPayload}};
                {'EXIT', Reason1} ->
                    stop(Reason1);
                {stop, Reason1} ->
                    stop(Reason1)
            end
    end;

%--------------------------------------------------------------------
%   All state events
%
handle_event(cast, {switch_piece, NewPieceId, NewPieceSize}, _AnyState, SD) ->
    #data{
        request_length = RequestLength,
        socket         = Socket
    } = SD,
    LastBlockId = trunc(math:ceil(NewPieceSize / RequestLength)),
    Blocks      = lists:seq(0, LastBlockId - 1),
    NewData0 = SD#data{
        piece_id             = NewPieceId,
        piece_size           = NewPieceSize,
        blocks               = Blocks,
        blocks_not_requested = Blocks,
        rest_payload         = undefined
    },
    SD0 = request_piece(NewData0),
    ok = gen_bittorrent_helper:get_packet(Socket),
    {keep_state, SD0};

%
%
handle_event({timeout, global}, timeout, _AnyState, _SD) ->
    {stop, timeout};

%
%
handle_event({call, From}, {call, Request}, _AnyState, SD = #data{cb_mod = CbMod, cb_state = CbState}) ->
    case catch CbMod:handle_call(Request, From, CbState) of
        {reply, Reply, NewCbState} ->
            {keep_state, SD#data{cb_state = NewCbState}, [{reply, From, Reply}]};
        {reply, Reply, NewCbState, NewTimeout} ->
            {keep_state, SD#data{cb_state = NewCbState}, [{reply, From, Reply}, {{timeout, global}, NewTimeout, timeout}]};
        {stop, Reason, Reply} ->
            stop(Reason);
        {'EXIT', Reason} ->
            stop(Reason)
    end;

%
%
handle_event(info, tcp_closed, _AnyState, _SD) ->
    {stop, {socket_error, tcp_closed}};

%
%
handle_event(info, Other, _AnyState, SD = #data{cb_mod = CbMod, cb_state = CbState}) ->
    case catch CbMod:handle_info(Other, CbState) of
        {ok, NewCbState} ->
            {keep_state, SD#data{cb_state = NewCbState}};
        {stop, Reason} ->
            stop(Reason);
        {'EXIT', Reason} ->
            stop(Reason)
    end.



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
                    PieceIdBin = gen_bittorrent_helper:int_to_bin32(PieceId),
                    PieceSizeBin = <<NextLength:32>>,
                    Message = gen_bittorrent_message:create_pipeline_request_piece(MsgAcc, PieceIdBin, OffsetBin, PieceSizeBin),
                    NewCbStateAcc = case catch CbMod:block_requested(PieceId, OffsetBin, NextLength, CbStateAcc) of
                        {ok, NewCbState} -> NewCbState;
                        {stop, Reason}   -> stop(Reason);
                        {'EXIT', Reason} -> stop(Reason)
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
        request_length  = RequestLength,
        cb_mod          = CbMod
    } = Data,
    {NewCbState, NewBlocks} = lists:foldl(
        fun
            ({piece, PD = #piece_data{}}, Acc = {AccCbState, AccBlocksLeft}) ->
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
                                {NewCbState1, NewAccBlocksLeft};
                            {stop, Reason} ->
                                stop(Reason);
                            {'EXIT', Reason} ->
                                stop(Reason)
                        end;
                    false ->
                        Acc
                end;
           (_Else, Acc) ->
               Acc
        end,
        {CbState0, Blocks},
        ParsedPayload
    ),
    Data#data{
        cb_state = NewCbState,
        blocks   = NewBlocks
    }.

