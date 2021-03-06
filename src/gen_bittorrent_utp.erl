%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2020, sarunas.bartusevicius@gmail.com
%%% @doc
%%% uTP (https://www.bittorrent.org/beps/bep_0029.html) implementation.
%%% @end
%%% Created : 17. Sep 2020 21.09
%%%-------------------------------------------------------------------
-module(gen_bittorrent_utp).
-author("bartimaeus").

-behaviour(gen_statem).

%% API
-export([
    start_link/4,
    start_link/5,
    start/4,
    start/5,
    get_local_port/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-ifdef(TEST).
-export([
    st_syn/1
]).
-endif.

-define(ST_DATA, <<16#01>>).
-define(ST_FIN, <<16#11>>).
-define(ST_STATE, <<16#21>>).
-define(ST_RESET, <<16#31>>).
-define(ST_SYN, <<16#41>>).

-define(EXTENSION, <<0>>).

-define(DEFAULT_WND_SIZE, <<0,0,0,0>>).

-record(state, {
    active          = false     :: false | once,
    type                        :: seeder | leecher,
    parent                      :: pid(),   % Start of this process
    ip                          :: inet:ip_address(),
    port                        :: inet:port_number(),
    socket                      :: port(),
    payload         = <<>>      :: binary(), % collected payload
    conn_id_recv                :: binary(), % rand()
    conn_id_send                :: binary(), % conn_id_recv + 1
    seq_nr                      :: binary(),
    last_ack_nr                 :: binary(),
    received_not_acked    = []  :: [binary()], % Received sequence numbers to which I am still not acked
    sent_not_acked        = []  :: [binary()], % Sent sequence numbers to which peer is still not acked (in-flight)
    curr_wnd_size               :: binary(),
    max_wnd_size                :: binary()
}).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Starts the server with a link.
%%  @end
-spec start_link(
    PeerIp          :: inet:ip_address(),
    PeerPort        :: inet:port_number(),
    Parent          :: pid(),
    SeederOrLeecher :: seeder | leecher
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_link(PeerIp, PeerPort, Parent, SeederOrLeecher) ->
    gen_statem:start_link(?MODULE, [PeerIp, PeerPort, Parent, SeederOrLeecher, 0], []).


%%  @doc
%%  Starts the server with a link.
%%  @end
-spec start_link(
    PeerIp          :: inet:ip_address(),
    PeerPort        :: inet:port_number(),
    Parent          :: pid(),
    SeederOrLeecher :: seeder | leecher,
    LocalPort       :: inet:port_number()
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_link(PeerIp, PeerPort, Parent, SeederOrLeecher, LocalPort) ->
    gen_statem:start_link(?MODULE, [PeerIp, PeerPort, Parent, SeederOrLeecher, LocalPort], []).


%%  @doc
%%  Starts the server without a link.
%%  @end
-spec start(
    PeerIp          :: inet:ip_address(),
    PeerPort        :: inet:port_number(),
    Parent          :: pid(),
    SeederOrLeecher :: seeder | leecher
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start(PeerIp, PeerPort, Parent, SeederOrLeecher) ->
    gen_statem:start(?MODULE, [PeerIp, PeerPort, Parent, SeederOrLeecher, 0], []).


%%  @doc
%%  Starts the server without a link.
%%  @end
-spec start(
    PeerIp          :: inet:ip_address(),
    PeerPort        :: inet:port_number(),
    Parent          :: pid(),
    SeederOrLeecher :: seeder | leecher,
    LocalPort       :: inet:port_number()
) ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start(PeerIp, PeerPort, Parent, SeederOrLeecher, LocalPort) ->
    gen_statem:start(?MODULE, [PeerIp, PeerPort, Parent, SeederOrLeecher, LocalPort], []).


%%  @doc
%%  Get local socket port.
%%  @end
-spec get_local_port(
    Pid :: pid()
) ->
    {ok, Port :: inet:port_number()}.

get_local_port(Pid) ->
    gen_statem:call(Pid, get_port).


%%  @doc
%%  Stops the client.
%%  @end
-spec stop(
    Pid :: pid()
) ->
    ok.

stop(Pid) ->
    gen_statem:stop(Pid).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%
%%
%%
init([PeerIp, PeerPort, Parent, SeederOrLeecher, LocalPort]) ->
    {ok, Socket} = gen_bittorrent_helper:open_udp_socket(LocalPort),
    RecvConnId = gen_bittorrent_helper:generate_random_binary(2),
    RecvConnIdInt = gen_bittorrent_helper:bin16_to_int(RecvConnId) + 1,
    SendConnId = gen_bittorrent_helper:int_to_bin16(RecvConnIdInt),
    State = #state{
        type         = SeederOrLeecher,
        socket       = Socket,
        parent       = Parent,
        ip           = PeerIp,
        port         = PeerPort,
        conn_id_send = SendConnId,
        conn_id_recv = RecvConnId,
        seq_nr       = <<(gen_bittorrent_helper:generate_random_binary(1))/binary, 0>>
    },
    case SeederOrLeecher of
        seeder  ->
            {ok, cs_syn_recv, State};
        leecher ->
            {ok, init, State, [{next_event, internal, connect}]}
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
%   CS_SYN_RECV state (for seeder)
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_syn_recv, SD) ->
    {next_state, cs_connected, SD#state{}};

%--------------------------------------------------------------------
%   Init state (for leecher)
%
handle_event(internal, connect, init, SD0) ->
    #state{
        socket          = Socket,
        ip              = Ip,
        port            = Port,
        sent_not_acked  = CurrSentNotAcked
    } = SD0,
    SD1 = #state{seq_nr = NewSqNr} = increase_seq_nr(SD0),
    SynPayload = st_syn(SD1),
    ok = socket_send(Socket, Ip, Port, SynPayload),
    SD2 = SD1#state{sent_not_acked = [NewSqNr | CurrSentNotAcked]},
    {next_state, cs_syn_sent, SD2, [{state_timeout, 60000, stop}]};

%--------------------------------------------------------------------
%   CS_SYN_SENT state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, <<16#21, RestMessage/binary>>}, cs_syn_sent, SD0) ->
    #state{
        conn_id_recv        = MyConnIdRecv,
        sent_not_acked      = CurrSentNotAcked,
        received_not_acked  = CurrReceivedNotAcked
    } = SD0,
    case RestMessage of
        <<_Extension:1/binary,
        ReceivedConnId:2/binary,
        TsMS:4/binary,
        TsDiffMS:4/binary,
        WndSize:4/binary,
        SeqNr:2/binary,
        AckNr:2/binary>> ->
            case MyConnIdRecv =:= ReceivedConnId    % Connection ID must be the same
                andalso lists:member(AckNr, CurrSentNotAcked)   % Must not be asked yet
            of
                true ->
                    SD1 = SD0#state{
                        max_wnd_size        = WndSize,
                        sent_not_acked      = CurrSentNotAcked -- [AckNr],
                        received_not_acked  = [SeqNr | CurrReceivedNotAcked]
                    },
                    {next_state, cs_connected, SD1};
                false ->
                    keep_state_and_data
            end;
        _ ->
            keep_state_and_data
    end;

handle_event(state_timeout, stop, cs_syn_sent, _SD) ->
    {stop, peer_not_responding};

%--------------------------------------------------------------------
%   CS_CONNECTED state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_connected, _SD) ->
    keep_state_and_data;

%--------------------------------------------------------------------
%   Any state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, <<16#31, RestMessage/binary>>}, _, SD) ->
    #state{conn_id_recv = MyConnIdRecv} = SD,
    <<_Extension:1/binary,
    ReceivedConnId:2/binary,
    _Rest/binary>> = RestMessage,
    case MyConnIdRecv =:= ReceivedConnId of
        true ->
            {next_state, terminated, SD};
        false ->
            keep_state_and_data
    end;

handle_event({call, From}, get_port, _, #state{socket = Socket}) ->
    {ok, LocalPort} = inet:port(Socket),
    {keep_state_and_data, [{reply, From, {ok, LocalPort}}]}.


%%%===================================================================
%%% Messages
%%%===================================================================

st_syn(#state{conn_id_send = ConnIdSend, seq_nr = SeqNr}) ->
    <<?ST_SYN/binary,
    ?EXTENSION/binary,
    ConnIdSend/binary,
    (gen_bittorrent_helper:get_timestamp_microseconds())/binary,
    0,0,0,0,
    ?DEFAULT_WND_SIZE/binary,
    SeqNr/binary,
    0,0>>.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%  @private
%%  @doc
%%  Send packets via UDP socket.
%%  @end
-spec socket_send(
    Socket  :: port(),
    Ip      :: inet:ip_address(),
    Port    :: inet:port_number(),
    Payload :: binary()
) -> ok.

socket_send(Socket, Ip, Port, Payload) ->
    socket_send(Socket, Ip, Port, Payload, 50).

socket_send(Socket, Ip, Port, Payload, Retries) ->
    case gen_udp:send(Socket, Ip, Port, Payload) of
        ok ->
            ok;
        {error, einval} -> % Received IP or port can be malformed
            ok;
        {error, eagain} -> % System call failed
            case Retries > 0 of
                true ->
                    timer:sleep(1000),
                    socket_send(Socket, Ip, Port, Payload, Retries - 1);
                false ->
                    {error, eagain}
            end;
        {error, enetunreach} -> % Network is unreachable
            case Retries > 0 of
                true ->
                    timer:sleep(5000),
                    socket_send(Socket, Ip, Port, Payload, Retries - 1);
                false ->
                    {error, enetunreach}
            end
    end.


%%  @private
%%  @doc
%%  Increase sequence number.
%%  @end
increase_seq_nr(SD = #state{seq_nr = SeqNr}) ->
    SeqNrInt = gen_bittorrent_helper:bin16_to_int(SeqNr) + 1,
    NewSeqNr = gen_bittorrent_helper:int_to_bin16(SeqNrInt),
    SD#state{seq_nr = NewSeqNr}.
