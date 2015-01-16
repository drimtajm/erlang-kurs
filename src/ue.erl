%%%-------------------------------------------------------------------
%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2015 by dreamtime <dreamtime@trillium>
%%%-------------------------------------------------------------------
-module(ue).

-compile(export_all).

-define(NAME, trillium).
-define(call(Pid, Call),
	begin
	    Pid ! {Call, self()},
	    receive
		{reply, Reply} -> Reply
	    end
	end).
-define(reply(Pid, Reply),
	Pid ! {reply, Reply}).

-on_load(on_load/0).

on_load() ->
    spawn(ue, init, []),
    ok.

%%%==============================================================
%%% API
%%%==============================================================

connect() ->
    ?call(mk_proc_name(), connect).

disconnect() ->
    ?call(mk_proc_name(), disconnect).

stop() ->
    ?call(mk_proc_name(), stop).

%%%==============================================================

init() ->
    register(mk_proc_name(), self()),
    loop(disconnected).

loop(State) ->
    receive
	{connect, From} when State == disconnected ->
	    Result = do_connect(),
	    ?reply(From, Result),
	    loop(connected);
	{disconnect, From} when State == connected ->
	    Result = do_disconnect(),
	    ?reply(From, Result),
	    loop(disconnected);
	{stop, From} ->
	    ?reply(From, ok),
	    ok;
	{Msg, From} ->
	    io:format("Received invalid message ~p when in state ~p~n",
		      [Msg, State]),
	    ?reply(From, {error, invalid_message}),
	    loop(State)
    end.

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
