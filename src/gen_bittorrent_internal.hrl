%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% Internal state structure.
%%% @end
%%% Created : 02. Aug 2019 14.48
%%%-------------------------------------------------------------------
-author("bartimaeus").

-define(DEFAULT_REQUEST_LENGTH, 16384).
-define(DEFAULT_PROTOCOL, tcp).
-define(DEFAULT_CONNECT_TIMEOUT, 10000).

-record(data, {
    % BitTorrent specific fields
    socket                                              :: port(),
    torrent_hash                                        :: binary(),
    peer_ip                                             :: inet:ip_address(),
    peer_port                                           :: inet:port_number(),
    piece_id                                            :: non_neg_integer(),
    piece_size                                          :: pos_integer(),
    blocks                                              :: [non_neg_integer()],
    blocks_not_requested                                :: [non_neg_integer()],
    connect_timeout         = ?DEFAULT_CONNECT_TIMEOUT  :: integer(),
    rest_payload                                        :: payload() | binary,
    protocol                = ?DEFAULT_PROTOCOL         :: tcp | udp,
    peer_id                                             :: string(),
    request_length          = ?DEFAULT_REQUEST_LENGTH   :: pos_integer(), % 16 kb
    % Process specific fields
    name                                                :: term(),
    cb_mod                                              :: module(),
    cb_state                                            :: term(),
    args                                                :: [term()],
    parent                                              :: pid(),
    debug                                               :: [term()],
    hibernate_timeout                                   :: timeout(),
    timeout                 = infinity                  :: timeout()
}).

-record(state, {
    handshake       = not_handshaked    :: not_handshaked | handshaked,
    leecher_state   = not_interested    :: not_interested | interested,
    peer_state      = choked            :: unchoked | choked
}).