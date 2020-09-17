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
-define(DEFAULT_RETRIES, 3).
-define(RETRY_TIMEOUT, 3000).
