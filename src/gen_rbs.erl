%%%-------------------------------------------------------------------
%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2015 by dreamtime <dreamtime@trillium>
%%%-------------------------------------------------------------------
-module(gen_rbs).

-behaviour(gen_server).

%% API
-export([start_link/0, dump_ue_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([handle/1]).

-define(SERVER, rbs).

-include("../include/gen_rbs.hrl").
-include("../include/events.hrl").
-include("../include/EUTRA-RRC-Definitions.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

dump_ue_info() ->
    gen_server:call(?SERVER, get_ue_info).


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
init([]) ->
    {ok, Pid} = tcpserver:start_server(fun handle/1),
    {ok, #state{tcpserver_pid=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_ue_info, _From, State) ->
    Reply = State#state.ues,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
%handle_info(_Info, State) ->
%    {noreply, State}.

handle_info(
  {#'RRC-UL-CCCH-Message'{
      message =
	  {c1,{rrcConnectionRequest,
	       #'RRC-RRCConnectionRequest'{
		  criticalExtensions =
		      {'rrcConnectionRequest-r8',
		       #'RRC-RRCConnectionRequest-r8-IEs'{
			  'ue-Identity' =
			      {randomValue,{0,<<255,255,229,156,ID>>}}}}}}}},
   SocketHandler}, #state{ues=UEs}=State) ->
    io:format("RBS UE Connect '~p'~n", [ID]),
    NewState = State#state{ues=[ ID | UEs ]},
    SocketHandler ! rrclib:make_rrc_connection_setup(ID),
    fra:notify_fra(#event_connected_ue{id=ID}),
    {noreply, NewState};

handle_info({bye, UEName, UEProc}, #state{ues=UEs}=State) ->
    io:format("RBS UE Disconnect '~p'~n", [[UEName, UEProc]]),
    UEProc ! {see_you_later, UEName},
    fra:notify_fra(#event_disconnected_ue{id=UEProc}),
    NewState = State#state{ues=lists:delete({UEName, UEProc}, UEs)},
    {noreply, NewState};

handle_info(Else, State) ->
    io:format("RBS UE Unexpected message: ~p~n", [Else]),
    {noreply, State}.


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
myreceive(Socket, Pending) ->
    case erlang:decode_packet(asn1, Pending, []) of
	{ok, Packet, Rest} ->
	    {ok, Packet, Rest};
	{more, _} ->
	    {ok, Data} = gen_tcp:recv(Socket, 0),
	    myreceive(Socket, <<Pending/binary, Data/binary>>)
    end.

handle1(Socket, Rest) ->
    {ok, Packet, NewRest} = myreceive(Socket, Rest),
    rbs ! { decode_packet(Packet), self() },
    receive
	Reply -> gen_tcp:send(Socket, encode_message(Reply))
    end,
    handle1(Socket, NewRest).

handle(Socket) ->
    handle1(Socket, <<"">>).

encode_message(Msg) ->
    {ok, Packet} = 'EUTRA-RRC-Definitions':encode('DL-CCCH-Message', Msg),
    Packet.

decode_packet(Packet) ->
    {ok, Msg} = 'EUTRA-RRC-Definitions':decode('UL-CCCH-Message', Packet),
    Msg.
