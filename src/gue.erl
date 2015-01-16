%%%-------------------------------------------------------------------
%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2015 by dreamtime <dreamtime@trillium>
%%%-------------------------------------------------------------------
-module(gue).

-behaviour(gen_server).

%% API
-export([start_link/0, connect/0, disconnect/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NAME, this_ue).

-record(state, { state=disconnected :: atom() }).

%%%===================================================================
%%% API
%%%===================================================================

connect() ->
    gen_server:call(mk_proc_name(), connect).

disconnect() ->
    gen_server:call(mk_proc_name(), disconnect).

stop() ->
    gen_server:call(mk_proc_name(), stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mk_proc_name()}, ?MODULE, [], []).

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
    {ok, #state{state=disconnected}}.

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
%handle_call(_Request, _From, State) ->
%    Reply = ok,
%    {reply, Reply, State};

handle_call(connect, _From, State) ->
    Reply = do_connect(),
    {reply, Reply, State};

handle_call(disconnect, _From, State) ->
    Reply = do_disconnect(),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

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
do_connect() ->
    rbs ! {hello, ?NAME, self()},
    receive
	{welcome, ?NAME} -> ok
    end.

do_disconnect() ->
    rbs ! {bye, ?NAME, self()},
    receive
	{see_you_later, ?NAME} ->
	    ok
    end.

mk_proc_name() ->
    list_to_atom(lists:concat([ue, "_", ?NAME])).
