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

-define(ST_DATA, <<0>>).
-define(ST_FIN, <<1>>).
-define(ST_STATE, <<2>>).
-define(ST_RESET, <<3>>).
-define(ST_SYN, <<4>>).

-define(EXTENSION, <<0,0>>).

-define(VERSION, <<1>>).

-define(DEFAULT_WND_SIZE, <<0,0,0,0,0,0,0,0>>).

-record(state, {
    active          = false     :: false | once,
    type                        :: sender | receiver,
    parent                      :: pid(),   % Start of this process
    ip                          :: inet:ip_address(),
    port                        :: inet:port_number(),
    socket                      :: port(),
    payload         = <<>>      :: binary(), % collected payload
    conn_id_recv                :: binary(), % rand()
    conn_id_send                :: binary(), % conn_id_recv + 1
    wnd_size                    :: binary(),
    seq_nr                      :: binary(),
    last_ack_nr                 :: binary()
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
    State = #state{
        type         = SeederOrLeecher,
        socket       = Socket,
        parent       = Parent,
        ip           = PeerIp,
        port         = PeerPort,
        conn_id_send = gen_bittorrent_helper:generate_random_binary(4),
        seq_nr       = <<(gen_bittorrent_helper:generate_random_binary(2))/binary, 0, 0>>
    },
    case SeederOrLeecher of
        seeder  ->
            {ok, wait, State};
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
%   Wait state (for seeder)
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, wait, SD) ->
    {next_state, cs_connected, SD#state{}};

%--------------------------------------------------------------------
%   Init state (for leecher)
%
handle_event(internal, connect, init, SD) ->
    #state{
        socket = Socket,
        ip     = Ip,
        port   = Port
    } = SD,
    NewSD = increase_seq_nr(SD),
    SynPayload = st_syn(NewSD),
    ok = socket_send(Socket, Ip, Port, SynPayload),
    {next_state, cs_syn_sent, NewSD#state{}};

%--------------------------------------------------------------------
%   CS_SYN_SENT state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_syn_sent, SD) ->
    {next_state, cs_connected, SD#state{}};

%--------------------------------------------------------------------
%   CS_CONNECTED state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_connected, _SD) ->
    keep_state_and_data;

%--------------------------------------------------------------------
%   Any state
%
handle_event({call, From}, get_port, _, #state{socket = Socket}) ->
    {ok, LocalPort} = inet:port(Socket),
    {keep_state_and_data, [{reply, From, {ok, LocalPort}}]}.


%%%===================================================================
%%% Messages
%%%===================================================================

st_syn(#state{conn_id_send = ConnIdSend, seq_nr = SeqNr}) ->
    <<?ST_SYN/binary,
    ?VERSION/binary,
    ?EXTENSION/binary,
    ConnIdSend/binary,
    (gen_bittorrent_helper:get_timestamp_microseconds())/binary,
    0,0,0,0,0,0,0,0,
    SeqNr/binary,
    0,0,0,0>>.

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
    SeqNrInt = gen_bittorrent_helper:bin32_to_int(SeqNr) + 1,
    NewSeqNr = gen_bittorrent_helper:int_to_bin32(SeqNrInt),
    SD#state{seq_nr = NewSeqNr}.
