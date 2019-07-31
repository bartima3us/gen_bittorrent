%%%-------------------------------------------------------------------
%%% @author bartimaeus
%%% @copyright (C) 2019, sarunas.bartusevicius@gmail.com
%%% @doc
%%% BitTorrent server mock.
%%% @end
%%% Created : 30. Jul 2019 22.09
%%%-------------------------------------------------------------------
-module(gen_bittorrent_server_mock).
-author("bartimaeus").

-behaviour(gen_server).

-include("gen_bittorrent.hrl").

%% API
-export([start_link/5]).

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
    port                    :: inet:port_number(),
    socket                  :: port(),
    rest                    :: undefined | binary(),
    peer_id                 :: string(), % @todo change to binary?
    torrent_hash            :: string(), % @todo change to binary?
    handshake       = false :: boolean(),
    unchoke         = false :: boolean(),
    event_mgr_pid           :: pid(),
    io_device,              % @todo import io_device() type
    piece_size              :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Port, PeerId, TorrentHash, File, PieceSize) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, PeerId, TorrentHash, File, PieceSize], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
init([Port, PeerId, TorrentHash, File, PieceSize]) ->
    self() ! start,
    {ok, EventMgrPid} = gen_event:start_link(),
    gen_event:add_handler(EventMgrPid, gen_bittorrent_event_handler, []),
    {ok, IoDevice} = file:open(File, [read, binary]),
    NewState = #state{
        port          = Port,
        peer_id       = PeerId,
        torrent_hash  = TorrentHash,
        event_mgr_pid = EventMgrPid,
        io_device     = IoDevice,
        piece_size    = PieceSize
    },
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(start, State) ->
    #state{
        port          = Port,
        event_mgr_pid = EventMgrPid
    } = State,
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, binary]),
    ok = gen_event:notify(EventMgrPid, server_started),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    {noreply, State#state{socket = AcceptSocket}};

%%
%%
handle_info({tcp, Socket, Payload}, State = #state{handshake = false, unchoke = false}) ->
    #state{
        rest         = Rest,
        peer_id      = PeerId,
        torrent_hash = TorrentHash
    } = State,
    {ok, ParsedPayload, NewRest} = gen_bittorrent_packet:parse(Payload, Rest),
    Handshake = case proplists:get_value(handshake, ParsedPayload, false) of
        true  ->
            ok = gen_bittorrent_message:handshake(Socket, PeerId, TorrentHash),
            true;
        false ->
            false
    end,
    ok = gen_bittorrent_helper:get_packet(Socket),
    {noreply, State#state{rest = NewRest, handshake = Handshake}};

%%
%%
handle_info({tcp, Socket, Payload}, State = #state{handshake = true, unchoke = false}) ->
    #state{
        rest = Rest
    } = State,
    {ok, ParsedPayload, NewRest} = gen_bittorrent_packet:parse(Payload, Rest),
    Unchoke = case proplists:get_value(interested, ParsedPayload, false) of
        true  ->
            ok = gen_bittorrent_message:unchoke(Socket),
            true;
        false ->
            false
    end,
    ok = gen_bittorrent_helper:get_packet(Socket),
    {noreply, State#state{rest = NewRest, unchoke = Unchoke}};

%%
%%
handle_info({tcp, Socket, Payload}, State = #state{handshake = true, unchoke = true}) ->
    #state{
        rest       = Rest,
        io_device  = IoDevice,
        piece_size = PieceSize
    } = State,
    {ok, ParsedPayload, NewRest} = gen_bittorrent_packet:parse(Payload, Rest),
    ok = lists:foreach(
        fun ({request, RequestData}) ->
            #request_data{
                piece_index  = PieceId,
                block_offset = OffsetBin,
                length       = Length
            } = RequestData,
            PieceIdBin = gen_bittorrent_helper:int_piece_id_to_bin(PieceId),
            OffsetInt = gen_bittorrent_helper:bin_piece_id_to_int(OffsetBin),
            LengthInt = gen_bittorrent_helper:bin_piece_id_to_int(Length),
            Offset = PieceSize * PieceId + OffsetInt,
            {ok, Block} = file:pread(IoDevice, {bof, Offset}, LengthInt),
            ok = gen_bittorrent_message:piece(Socket, PieceIdBin, OffsetBin, Block)
        end,
        ParsedPayload
    ),
    ok = gen_bittorrent_helper:get_packet(Socket),
    {noreply, State#state{rest = NewRest}};

%%
%%
handle_info({tcp_closed, _Socket}, State = #state{io_device = IoDevice}) ->
    ct:print("Server socket closed."),
    ok = file:close(IoDevice),
    {noreply, State};

%%
%%
handle_info(Info, State) ->
    ct:print("Info=~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Termination callback.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    #state{
        socket    = Socket,
        io_device = IoDevice
    } = State,
    ok = file:close(IoDevice),
    ok = case Socket of
        Socket when is_port(Socket) -> gen_tcp:close(Socket);
        _Else -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
