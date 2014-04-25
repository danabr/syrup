%% @doc Tests for the syrup module.
%% @copyright 2014 Daniel Abrahamsson
%% Licensed under the MIT license.
-module(syrup_tests).

-export([ ping/2 ]).

%% Syrup callback for tests
ping(Who, PongMsg) -> Who ! PongMsg.

%% Tests
-include_lib("eunit/include/eunit.hrl").
-define(SYRUP_FILE, "test/syrup.db").

syrup_test_() ->
  {timeout, 60, { foreach
  , fun setup/0
  , fun teardown/1
  , [ fun test_single_task/1
    , fun test_multiple_tasks/1
    , fun test_stop_before_task_is_started/1
    , fun test_kill_before_task_is_started/1
    ]}}.

setup() ->
  file:delete(?SYRUP_FILE),
  {ok, Pid} = start_syrup(),
  Pid.

teardown(_) ->
  syrup:stop().

test_single_task(_) ->
  ?_test(begin
    syrup:delay(1, syrup_tests, ping, [self(), message_received]),
    assert_no_message_received(900),
    assert_received(200, message_received)
  end).

test_multiple_tasks(_) ->
  ?_test(begin
    syrup:delay(2, syrup_tests, ping, [self(), message1]),
    syrup:delay(1, syrup_tests, ping, [self(), message2]),
    assert_no_message_received(900),
    assert_received(200, message2),
    assert_received(1000, message1)
  end).

test_stop_before_task_is_started(_) ->
  ?_test(begin
    syrup:delay(1, syrup_tests, ping, [self(), message_received]),
    syrup:stop(),
    assert_no_message_received(1200),
    {ok, _} = start_syrup(),
    assert_received(100, message_received)
  end).

test_kill_before_task_is_started(SyrupPid) ->
  ?_test(begin
    syrup:delay(1, syrup_tests, ping, [self(), message_received]),
    kill_pid(SyrupPid),
    assert_no_message_received(1200),
    {ok, _} = start_syrup(),
    assert_received(100, message_received)
  end).

assert_received(ExpectedDelay, ExpectedMsg) ->
  receive
    ExpectedMsg -> ok;
    OtherMsg    ->
      error({unexpected_message, [{expected, ExpectedMsg}, {got, OtherMsg}]})
  after ExpectedDelay ->
    error({no_message_received, {expected, ExpectedMsg}})
  end.

assert_no_message_received(Duration) ->
  receive
    Msg -> error({unexpected_message, Msg})
  after
    Duration -> ok
  end.

kill_pid(Pid) ->
  MonitorRef = monitor(process, Pid),
  exit(Pid, kill),
  receive
    {'DOWN', MonitorRef, _, _, _} -> ok
  after
    1000 -> error({failed_to_kill_pid, Pid})
  end.

start_syrup() -> syrup:start([{file, ?SYRUP_FILE}]).
