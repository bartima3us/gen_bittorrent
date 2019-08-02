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

This behaviour is still experimental.

## <a name="usage">Usage</a> ##

Implement gen_bittorrent behaviour (ex.: examples/gen_bittorrent_impl.erl).
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


More complex example can be found: https://github.com/bartima3us/erl-bittorrent

## <a name="tests">Tests</a> ##

EUnit and CT tests
```
$ make tests
```
