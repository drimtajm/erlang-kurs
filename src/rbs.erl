%%% @author dreamtime <dreamtime@trillium>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2015 by dreamtime <dreamtime@trillium>

-module(rbs).

-compile(export_all).

handler(State) ->
    receive
	{hello, UEName, UEProc} ->
	    
	    io:format("RBS UE Connect '~p'~n", [[UEName, UEProc]]),
	    NewState = [ UEName, UEProc ],
	    UEProc ! { welcome, UEName },
	    handler(NewState);
	{bye, UEName, UEProc} when State == [UEName, UEProc]->
	    io:format("RBS UE Disconnect '~p'~n", [[UEName, UEProc]]),
	    UEProc ! {see_you_later, UEName},
	    NewState = [],
	    handler(NewState);
	{bye, UEName, UEProc} ->
	    io:format("RBS UE Disconnect from unknown UE '~p' ~n",
		      [[UEName, UEProc]]),
		handler(State);
	AnyDebug ->
	    io:format("RBS Debug: ~p~n", [AnyDebug])
    end.

init() ->
    Pid = spawn_link(rbs, handler, [[]]),
    register(rbs, Pid).



