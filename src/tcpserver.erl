%%%-------------------------------------------------------------------
%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2015 by dreamtime <dreamtime@trillium>
%%%-------------------------------------------------------------------
-module(tcpserver).

-export([start_server/1, stop_server/1]).

start_server(HandlerFunc) ->
    Port = 30000,
    Pid = spawn_link(fun() ->
			     {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
			     spawn(fun() -> acceptor(HandlerFunc, Listen) end),
			     receive
				     stop -> ok
			     end
		     end),
    {ok, Pid}.

stop_server(Pid) ->
    Pid ! stop,
    ok.

acceptor(HandlerFunc, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() ->
		  inet:setopts(Socket, [{active, false}]),
		  HandlerFunc(Socket) end),
    acceptor(HandlerFunc, ListenSocket).

