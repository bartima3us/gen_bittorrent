%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2018, sarunas.bartusevicius@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 21.17
%%%-------------------------------------------------------------------
-module(gen_bittorrent_packet).
-author("bartimaeus").

-include("gen_bittorrent.hrl").

%% API
-export([
    parse/2
]).

-ifdef(TEST).
-export([
    identify/1,
    parse_bitfield/1
]).
-endif.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Start parsing data (sync. call)
%%
parse(Data, Rest) ->
    {FullData, MessageType, ExtraData} = case Rest of
        Rest when is_binary(Rest) ->
            {<<Rest/binary, Data/binary>>, undefined, undefined};
        undefined ->
            {Data, undefined, undefined}
    end,
    case MessageType of
        undefined   -> identify(FullData);
        MessageType -> identify(MessageType, {FullData, ExtraData})
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Identify and parse packets
%%
identify(Data) ->
    identify(Data, []).

identify(<<>>, Acc) ->
    {ok, lists:reverse(Acc), undefined};

% @todo maybe need to remove second elements of tuple (true)...
% Handshake
identify(<<19, _Label:19/bytes, _ReservedBytes:8/bytes, _Hash:20/bytes, _PeerId:20/bytes, Rest/bytes>>, Acc) ->
    identify(Rest, [{handshake, true} | Acc]);

%
% Keep alive
identify(<<0, 0, 0, 0, Rest/bytes>>, Acc) ->
    identify(Rest, [{keep_alive, true} | Acc]);

%
% Choke
identify(<<0, 0, 0, 1, 0, Rest/bytes>>, Acc) ->
    identify(Rest, [{choke, true} | Acc]);

%
% Uncoke
identify(<<0, 0, 0, 1, 1, Rest/bytes>>, Acc) ->
    identify(Rest, [{unchoke, true} | Acc]);

%
% Interested
identify(<<0, 0, 0, 1, 2, Rest/bytes>>, Acc) ->
    identify(Rest, [{interested, true} | Acc]);

%
% Not interested
identify(<<0, 0, 0, 1, 3, Rest/bytes>>, Acc) ->
    identify(Rest, [{not_interested, true} | Acc]);

%
% Have (fixed length, always 0005)
identify(FullData = <<0, 0, 0, 5, 4, Data/bytes>>, Acc) ->
    PayloadLength = 4, % Because we've already matched Idx=4
    case Data of
        Data when byte_size(Data) < PayloadLength ->
            {ok, lists:reverse(Acc), FullData};
        Data ->
            <<Payload:PayloadLength/bytes, Rest/bytes>> = Data,
            identify(Rest, [{have, gen_bittorrent_helper:bin32_to_int(Payload)} | Acc])
    end;

%
% Bitfield
identify(FullData = <<Length:4/bytes, 5, Data/bytes>>, Acc) ->
    <<FullLength:32>> = Length,     % Convert to integer (same as: <<FullLength:32/integer>> = Length)
    PayloadLength = FullLength - 1, % Because we've already matched Idx=5
    case Data of
        Data when byte_size(Data) < PayloadLength ->
            {ok, lists:reverse(Acc), FullData};
        Data ->
            <<Payload:PayloadLength/binary, Rest/binary>> = Data,
            BitField = #bitfield_data{
                parsed  = parse_bitfield(Payload),
                payload = Payload,
                length  = Length
            },
            identify(Rest, [{bitfield, BitField} | Acc])
    end;

%
% Request (length = 13)
identify(FullData = <<0, 0, 0, 13, 6, Data/bytes>>, Acc) ->
    PayloadLength = 12, % Because we've already matched Idx=6
    case Data of
        Data when byte_size(Data) < PayloadLength ->
            {ok, lists:reverse(Acc), FullData};
        Data ->
            <<PieceIndex:4/bytes, BlockOffset:4/bytes, BlockLength:4/bytes, Rest/bytes>> = Data,
            Request = #request_data{
                piece_index  = gen_bittorrent_helper:bin32_to_int(PieceIndex),
                block_offset = BlockOffset,
                length       = BlockLength
            },
            identify(Rest, [{request, Request} | Acc])
    end;

%
% Piece (length = 16384 bytes (piece size) + 9 (piece: <len=0009+X><id=7><index><begin><block>))
identify(FullData = <<Length:4/bytes, 7, PieceIndex:4/bytes, BlockOffset:4/bytes, Data/bytes>>, Acc) ->
    <<FullLength:32>> = Length,      % Convert to integer
    PayloadLength = FullLength - 9,  % Because we've already matched Idx, PieceIndex and BlockOffset
    case Data of
        Data when byte_size(Data) < PayloadLength ->
            {ok, lists:reverse(Acc), FullData};
        Data ->
            <<Payload:PayloadLength/bytes, Rest/bytes>> = Data,
            Piece = #piece_data{
                payload      = Payload,
                length       = Length,
                piece_index  = gen_bittorrent_helper:bin32_to_int(PieceIndex),
                block_offset = BlockOffset
            },
            identify(Rest, [{piece, Piece} | Acc])
    end;

%
% Extended
identify(FullData = <<Length:4/bytes, 20, Data/bytes>>, Acc) ->
    <<FullLength:32>> = Length,     % Convert to integer (same as: <<FullLength:32/integer>> = Length)
    PayloadLength = FullLength - 1, % Because we've already matched Idx=20
    case Data of
        Data when byte_size(Data) < PayloadLength ->
            {ok, lists:reverse(Acc), FullData};
        Data ->
            <<Payload:PayloadLength/binary, Rest/binary>> = Data,
            identify(Rest, [{extended, gen_bittorrent_bencoding:decode(Payload)} | Acc])
    end;

identify(Data, Acc) ->
    {ok, lists:reverse(Acc), Data}.



%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @private
%% Parse bitfield to bits ({PieceId, true | false}). True or false depends if peer has a piece or not.
%%
parse_bitfield(Bitfield) when is_list(Bitfield) ->
    parse_bitfield(Bitfield, 0, []);

parse_bitfield(Bitfield) when is_binary(Bitfield) ->
    parse_bitfield(binary_to_list(Bitfield), 0, []).

parse_bitfield([], _Iteration, Acc) ->
    lists:reverse(lists:flatten(Acc));

parse_bitfield([Byte|Bytes], Iteration, Acc) ->
    <<B1:1/bits, B2:1/bits, B3:1/bits, B4:1/bits, B5:1/bits, B6:1/bits, B7:1/bits, B8:1/bits>> = <<Byte>>,
    ConvertFun = fun
        (<<1:1>>) -> true;
        (<<0:1>>) -> false
    end,
    Result = [
        {7 + Iteration, ConvertFun(B8)},
        {6 + Iteration, ConvertFun(B7)},
        {5 + Iteration, ConvertFun(B6)},
        {4 + Iteration, ConvertFun(B5)},
        {3 + Iteration, ConvertFun(B4)},
        {2 + Iteration, ConvertFun(B3)},
        {1 + Iteration, ConvertFun(B2)},
        {0 + Iteration, ConvertFun(B1)}
    ],
    parse_bitfield(Bytes, Iteration + 8, [Result|Acc]).


