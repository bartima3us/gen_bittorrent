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

% @todo specify types
-record(data, {
    % BitTorrent specific fields
    socket                  :: port(),
    torrent_hash,
    peer_ip,
    peer_port,
    piece_id,
    piece_size,
    blocks,
    blocks_not_requested,
    connect_timeout         = ?DEFAULT_CONNECT_TIMEOUT,
    rest_payload,
    protocol                = ?DEFAULT_PROTOCOL,
    peer_id,
    request_length          = ?DEFAULT_REQUEST_LENGTH, % 16 kb
    % Process specific fields
    name,
    cb_mod,
    cb_state,
    args,
    parent,
    debug,
    hibernate_timeout,
    timeout                 = infinity
}).

-record(state, {
    handshake       = not_handshaked    :: not_handshaked | handshaked,
    leecher_state   = not_interested    :: not_interested | interested,
    peer_state      = choked            :: unchoked | choked
}).