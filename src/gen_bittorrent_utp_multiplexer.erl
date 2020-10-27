%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2020, sarunas.bartusevicius@gmail.com
%%% @doc
%%% uTP (https://www.bittorrent.org/beps/bep_0029.html) seeder multiplexer.
%%% Multiplexer purpose is to forward received uTP data to appropriate FSM by connection id.
%%% @end
%%% Created : 27. Oct 2020 16.13
%%%-------------------------------------------------------------------
-module(gen_bittorrent_utp_multiplexer).
-author("bartimaeus").

-behaviour(gen_server).

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    socket          :: port(),
    local_port      :: inet:port_number(),
    conn_handlers   :: dict:dict()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(
    LocalPort :: inet:port_number()
) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.

start_link(LocalPort) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalPort], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LocalPort]) ->
    {ok, Socket} = gen_bittorrent_helper:open_udp_socket(LocalPort),
    State = #state{
        socket        = Socket,
        local_port    = LocalPort,
        conn_handlers = dict:new()
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Message = {udp, _Port, DstIp, DstPort, Payload}, State = #state{conn_handlers = ConnHandlers}) ->
    NewState = case erlang:byte_size(Payload) >= 40 of
        true ->
            <<_:16/binary, ConnId:16/binary, _/binary>> = Payload,
            case dict:find(ConnId, ConnHandlers) of
                {ok, {_, Pid}} ->
                    Pid ! Message;
                error ->
                    % @TODO: change gen_bittorrent_utp to gen_bittorrent later
                    {ok, Pid} = gen_bittorrent_utp:start(DstIp, DstPort, self(), seeder),
                    MonitorRef = erlang:monitor(process, Pid),
                    State#state{conn_handlers = dict:store(ConnId, {MonitorRef, Pid}, ConnHandlers)}
            end;
        false ->
            State
    end,
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
