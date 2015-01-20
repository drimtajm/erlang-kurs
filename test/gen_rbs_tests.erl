-module(gen_rbs_tests).

-include_lib("mockgyver/include/mockgyver.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/gen_rbs.hrl").

%% ---------------------------------------------------------------------
%% Test constants
%% ---------------------------------------------------------------------

gen_rbs_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun teardown/1).

setup() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun receiver_loop/0),
    ?FORGET_CALLS(gen_rbs_tests:receiver_loop()),
    {ok, Pid}.

teardown({ok, Pid}) ->
    Pid ! stop.

receiver_loop() ->
    receive
	{welcome, _} ->
	    receiver_loop();
	stop ->
	    ok
    end.

should_connect_ue_test({ok, Pid}) ->
    {noreply, NewState} = gen_rbs:handle_info({hello, dummy, Pid},
					      #state{ues=[]}),
    lists:member({dummy, Pid}, NewState#state.ues).

