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
    start_link/0,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4
]).

-define(ST_DATA, 0).
-define(ST_FIN, 1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN, 4).

-define(EXTENSION, <<"00">>).

-record(state, {
    active          = false :: false | once,
    ip,
    port,
    socket,
    conn_id_recv, % rand()
    conn_id_send, % conn_id_send = conn_id_recv + 1
    wnd_size,
    last_ack_nr
}).


%%%===================================================================
%%% API
%%%===================================================================

%%  @doc
%%  Starts the server.
%%
-spec start_link() ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: term()}.

start_link() ->
    gen_statem:start_link(?MODULE, [], []).


%%  @doc
%%  Stops the client.
%%
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
init([]) ->
    State = #state{},
    {ok, waiting, State, [{next_event, internal, start}]}.


%%
%%
%%
callback_mode() ->
    [handle_event_function].


%%%===================================================================
%%% States
%%%===================================================================

%--------------------------------------------------------------------
%   Init state
%
handle_event(internal, start, init, SD) ->
    {next_state, cs_syn_sent, SD#state{}};

%--------------------------------------------------------------------
%   CS_SYN_SENT state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_syn_sent, SD) ->
    {next_state, cs_syn_sent, SD#state{}};

%--------------------------------------------------------------------
%   CS_CONNECTED state
%
handle_event(info, {udp, _Port, _DstIp, _DstPort, _Message}, cs_connected, _SD) ->
    keep_state_and_data.

%%%===================================================================
%%% Internal functions
%%%===================================================================

