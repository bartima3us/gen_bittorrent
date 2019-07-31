gen_bittorrent
=====

- [Introduction](#introduction)
- [Status](#status)
- [Usage](#usage)
- [Tests](#tests)

## <a name="introduction">Introduction</a> ##

Generic gen_bittorrent behaviour implemented as OTP compliant special process FSM.
The purpose of this behaviour is to download a torrent file piece.
It can be used as a high level transport protocol for any software.
To avoid any unnecessary overhead, no generic behaviour was used.
gen_bittorrent relies only on proc_lib, sys, gen modules.

## <a name="status">Status</a> ##

This behaviour is highly experimental.
Currently without proper testing and decent documentation.

## <a name="usage">Usage</a> ##

Implement gen_bittorrent behaviour (ex.: examples/gen_bittorrent_impl.erl).
Starting:

```
gen_bittorrent:start_link(CbMod :: module(), PeerIp :: inet:ip_address(), PeerPort :: inet:port_number(), PeerId :: string(), TorrentHash :: binary(), PieceId :: non_neg_integer(), PieceSize :: integer(), Args :: list(), Opts :: list()).
```

Switching piece:

```
gen_bittorrent:switch_piece(Name :: term(), PieceId :: non_neg_integer(), PieceSize :: integer()).
```

More complex example can be found: https://github.com/bartima3us/erl-bittorrent

## <a name="tests">Tests</a> ##

EUnit and CT tests
```
$ make tests
```
