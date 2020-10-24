%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2017, sarunas.bartusevicius@gmail.com
%%% @doc
%%% BitTorrent helper functions.
%%% @end
%%% Created : 01. Jun 2017 15.59
%%%-------------------------------------------------------------------
-module(gen_bittorrent_helper).
-author("bartimaeus").

%% API
-export([
    urlencode/1,
    bin32_to_int/1,
    int_to_bin32/1,
    get_packet/1,
    generate_random_binary/1,
    get_timestamp_microseconds/0
]).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  PHP urlencode style function
%%
%%  432D7D1A 0B153F3F 5CE1B453 C367A5B5 5DF453E9
%%  C-%7D%1A%0B%15%3F%3F%5C%E1%B4S%C3g%A5%B5%5D%F4S%E9
%%  C - %7D %1A %0B %15 %3F %3F %5C %E1 %B4 S %C3 g %A5 %B5 %5D %F4 S %E9
%%  C   - 67     C
%%  -   - 45     -
%%  }   - 125    %
%%  SUB - 26     %
%%  VT  - 11     %
%%  NAK - 21     %
%%  ?   - 63     %
%%  ?   - 63     %
%%  \   - 92     %
%%  ß   - 225    %
%%  ┤   - 180    %
%%  S   - 83     S
%%  ├   - 195    %
%%  g   - 103    g
%%  Ñ   - 165    %
%%  Á   - 181    %
%%  ]   - 93     %
%%  ¶   - 244    %
%%  S   - 83     S
%%  Ú   - 233    %
%%  @end
urlencode(String) ->
    Value = case is_binary(String) of
    true -> binary_to_list(String);
        _ -> String
    end,
    %% Integers, uppercase chars, lowercase chars, - _
    AllowedSymbols = lists:seq(48, 57) ++ lists:seq(65, 90) ++ lists:seq(97, 122) ++ [45, 95],
    Parse = fun (Symbol) ->
        case lists:member(Symbol, AllowedSymbols) of
            true ->
                Symbol;
            _ ->
                HexList = httpd_util:integer_to_hexlist(Symbol),
                case string:len(HexList) of
                    1 -> "%0" ++ string:to_lower(HexList);
                    _ -> "%" ++ string:to_lower(HexList)
                end
        end
    end,
    lists:map(Parse, Value).


%%  @doc
%%  Convert from 32 bits binary to integer
%%  @end
bin32_to_int(Bin) when is_binary(Bin) ->
    <<Int:32>> = Bin,
    Int.

%% @doc
%% Convert from integer to 32 bits binary
%%
int_to_bin32(Int) when is_integer(Int) ->
    <<Int:32>>.


%%  @doc
%%  Make socket active once
%%  @end
get_packet(Socket) ->
    inet:setopts(Socket, [{active, once}]).


%%  @doc
%%  Generate random binary of the specified length in bytes.
%%  @end
-spec generate_random_binary(
    Length :: non_neg_integer()
) -> binary().

generate_random_binary(Length) ->
    crypto:strong_rand_bytes(Length).


%%  @doc
%%  Get last 4 bytes of timestamp in microseconds.
%%  @end
get_timestamp_microseconds() ->
    <<_:4/binary, TS/binary>> = <<(os:system_time()):64>>,
    TS.
