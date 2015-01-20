%%%-------------------------------------------------------------------
%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2015 by dreamtime <dreamtime@trillium>
%%%-------------------------------------------------------------------
-module(fra).

-behaviour(gen_server).

%% API
-export([start_link/0, notify_fra/1, subscribe/2, unsubscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {subscribers = []}).

%%%===================================================================
%%% API
%%%===================================================================

notify_fra(Data) ->
    gen_server:call(?SERVER, {notify_fra, Data}).

subscribe(Who, Event) ->
    gen_server:call(?SERVER, {subscribe, Who, Event}).

unsubscribe(Who, Event) ->
    gen_server:call(?SERVER, {unsubscribe, Who, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, #state{}}.

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
handle_call({notify_fra, Data}, _From, State) ->
    Subscribers = State#state.subscribers,
    EventName = element(1, Data),
    EventSubscribers = [Who || {Event, Who} <- Subscribers,
			       Event =:= EventName], 
    
    lists:foreach(fun(Subscriber)->
			  Subscriber ! Data
		  end,
		  EventSubscribers),
			  
    Reply = ok,
    {reply, Reply, Subscribers};

handle_call({subscribe, Who, Event}, _From, State) ->
    Subscribers = State#state.subscribers,
    NewSubscribers = [ { Event, Who } | Subscribers ],
    Reply = ok,
    {reply, Reply, State#state{subscribers = NewSubscribers}};

handle_call({unsubscribe, Who, Event}, _From, State) ->
    Subscribers = State#state.subscribers,
    NewSubscribers = Subscribers -- [{Event, Who}],
    Reply = ok,
    {reply, Reply, State#state{subscribers = NewSubscribers}}.

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
handle_info(_Info, State) ->
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
