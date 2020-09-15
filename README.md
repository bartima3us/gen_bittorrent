gen_bittorrent
=====

- [Introduction](#introduction)
- [Usage](#usage)
- [Tests](#tests)

## <a name="introduction">Introduction</a> ##

Generic `gen_bittorrent` behaviour implemented on top of OTP `gen_statem`.<br/>
The purpose of this behaviour is to download a torrent file piece.<br/>
It can be used as a transfer protocol for any software.<br/>
Supports all standard (BEP003) and extended (BEP0010) messages.

Library also includes:
- `gen_bittorrent_helper.erl` - helper functions for encoding and numbers converting.
- `gen_bittorrent_message.erl` - helper functions for forming BitTorrent messages.
- `gen_bittorrent_packet.erl` - helper functions for parsing BitTorrent messages.

## <a name="usage">Usage</a> ##

Behaviour implementation example: `examples/gen_bittorrent_impl.erl`<br/>
More complex usage example can be found: https://github.com/bartima3us/erl-bittorrent

Starting with/without link and without/without name:

```
gen_bittorrent:start_link(CbMod :: module(), PeerIp :: inet:ip_address(), PeerPort :: inet:port_number(), PeerId :: string(), TorrentHash :: binary(), PieceId :: non_neg_integer(), PieceSize :: non_neg_integer(), Args :: [term()], Opts :: [term()]).
```
```
gen_bittorrent:start_link(Name :: term(), CbMod :: module(), PeerIp :: inet:ip_address(), PeerPort :: inet:port_number(), PeerId :: string(), TorrentHash :: binary(), PieceId :: non_neg_integer(), PieceSize :: non_neg_integer(), Args :: [term()], Opts :: [term()]).
```
```
gen_bittorrent:start(CbMod :: module(), PeerIp :: inet:ip_address(), PeerPort :: inet:port_number(), PeerId :: string(), TorrentHash :: binary(), PieceId :: non_neg_integer(), PieceSize :: non_neg_integer(), Args :: [term()], Opts :: [term()]).
```
```
gen_bittorrent:start(Name :: term(), CbMod :: module(), PeerIp :: inet:ip_address(), PeerPort :: inet:port_number(), PeerId :: string(), TorrentHash :: binary(), PieceId :: non_neg_integer(), PieceSize :: non_neg_integer(), Args :: [term()], Opts :: [term()]).
```

Switch piece to the new one:
```
gen_bittorrent:switch_piece(Name :: term(), PieceId :: non_neg_integer(), PieceSize :: non_neg_integer()).
```

Send synchronous message:
```
gen_bittorrent:call(Name :: term(), Request :: term()).
```
```
gen_bittorrent:call(Name :: term(), Request :: term(), Timeout :: pos_integer()).
```

Stop the process:
```
gen_bittorrent:stop(Name :: term()).
```
```
gen_bittorrent:stop(Name :: term(), Reason :: term(), Timeout :: pos_integer()).
```

## <a name="tests">Tests</a> ##

EUnit and CT tests
```
$ make tests
```
