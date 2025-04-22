-module(counter_server_tests).

-include_lib("eunit/include/eunit.hrl").

counter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [fun test_increment/0, fun test_decrement/0, fun test_get/0]}.

setup() ->
    {ok, Pid} = counter_server:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(counter_server).

test_increment() ->
    Initial = counter_server:get(),
    counter_server:increment(),
    ?assertEqual(Initial + 1, counter_server:get()).

test_decrement() ->
    Initial = counter_server:get(),
    counter_server:decrement(),
    ?assertEqual(Initial - 1, counter_server:get()).

test_get() ->
    ?assert(is_integer(counter_server:get())).
