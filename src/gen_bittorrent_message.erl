%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2018, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2018 14.25
%%%-------------------------------------------------------------------
-module(gen_bittorrent_message).
-author("bartimaeus").

%% API
-export([
    handshake/3,
    keep_alive/1,
    choke/1,
    unchoke/1,
    interested/1,
    not_interested/1,
    have/2,
    bitfield/2,
    request_piece/2,
    request_piece/4,
    pipeline_request_piece/4,
    cancel/4
]).


%% @doc
%% Send `handshake` message
%% @todo change to binary?
handshake(Socket, PeerId, Hash) ->
    Request = [
        19,
        "BitTorrent protocol",
        0,0,0,0,0,0,0,0,
        Hash,
        PeerId
    ],
    gen_tcp:send(Socket, list_to_binary(Request)).


%% @doc
%% Send `keep alive` message
%%
keep_alive(Socket) ->
    gen_tcp:send(Socket, <<00, 00, 00, 00>>).


%% @doc
%% Send `choke` message
%%
choke(Socket) ->
    gen_tcp:send(Socket, <<00, 00, 00, 01, 00>>).


%% @doc
%% Send `unchoke` message
%%
unchoke(Socket) ->
    gen_tcp:send(Socket, <<00, 00, 00, 01, 01>>).


%% @doc
%% Send `interested` message
%%
interested(Socket) ->
    gen_tcp:send(Socket, <<00, 00, 00, 01, 02>>).


%% @doc
%% Send `not_interested` message
%%
not_interested(Socket) ->
    gen_tcp:send(Socket, <<00, 00, 00, 01, 03>>).


%% @doc
%% Send `have` message
%%
have(Socket, PieceId) ->
    gen_tcp:send(Socket, <<00, 00, 00, 05, 04, PieceId/binary>>).


%% @doc
%% Send `bitfield` message
%%
bitfield(Socket, Bitfield) ->
    SizeInt = byte_size(Bitfield) + 1,
    SizeBin = <<SizeInt:32>>,
    gen_tcp:send(Socket, <<SizeBin/binary, 05, Bitfield/binary>>).


%% @doc
%% Send `request piece` message
%%
request_piece(Socket, PieceId, PieceBegin, PieceLength) when is_integer(PieceId) ->
    PieceIdBin = gen_bittorrent_helper:int_piece_id_to_bin(PieceId),
    request_piece(Socket, PieceIdBin, PieceBegin, PieceLength);

request_piece(Socket, PieceId, PieceBegin, PieceLength) when is_binary(PieceId) ->
    PieceLengthBin = <<PieceLength:32>>, % @todo move to generic helper
    gen_tcp:send(
        Socket,
        <<
            00, 00, 00, 16#0d,      % Message length
            06,                     % Message type
            PieceId/binary,         % Piece index
            PieceBegin/binary,      % Begin offset of piece
            PieceLengthBin/binary   % Piece length
        >>
    ).

request_piece(Socket, Message) ->
    gen_tcp:send(Socket, Message).


%%  @doc
%%  Concat `request piece` messages for pipelining.
%%
pipeline_request_piece(MsgAcc, PieceIdBin, OffsetBin, PieceLengthBin) ->
    <<
        MsgAcc/binary,
        00, 00, 00, 16#0d,      % Message length
        06,                     % Message type
        PieceIdBin/binary,      % Piece index
        OffsetBin/binary,       % Begin offset of piece
        PieceLengthBin/binary   % Piece length
    >>.


%% @doc
%% Send `cancel` message
%%
cancel(Socket, PieceId, PieceBegin, PieceLength) when is_integer(PieceId) ->
    PieceIdBin = gen_bittorrent_helper:int_piece_id_to_bin(PieceId),
    cancel(Socket, PieceIdBin, PieceBegin, PieceLength);

cancel(Socket, PieceId, PieceBegin, PieceLength) when is_binary(PieceId) ->
    PieceLengthBin = <<PieceLength:32>>,
    gen_tcp:send(
        Socket,
        <<
            00, 00, 00, 16#0d,      % Message length
            08,                     % Message type
            PieceId/binary,         % Piece index
            PieceBegin/binary,      % Begin offset of piece
            PieceLengthBin/binary   % Piece length
        >>
    ).


