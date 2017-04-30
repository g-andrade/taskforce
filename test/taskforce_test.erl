-module(taskforce_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


-spec calculate_primes_test() -> ok.
calculate_primes_test() ->
    application:ensure_all_started(taskforce),

    NrOfPrimes = 200,
    ExpectedNthPrimes = [{Nth, find_nth_prime_recur(Nth)}
                         || Nth <- lists:seq(1, 200)],

    IndividualTimeoutT = 2000, % in miliseconds
    GlobalTimeoutT = 10000,    % in miliseconds
    MaxWorkers = 4,        % 4 workers
    Tasks =
        maps:from_list(
          [{Nth, taskforce:task(fun find_nth_prime/1, [Nth],
                                #{ timeout => IndividualTimeoutT })}
           || Nth <- lists:seq(1, NrOfPrimes)]),

    Result = taskforce:execute(Tasks, #{ timeout => GlobalTimeoutT,
                                         max_workers => MaxWorkers }),

    #{ completed := NthPrimes,
       individual_timeouts := IndividualTimeouts,
       global_timeouts := GlobalTimeouts } = Result,

    ?assertEqual([], IndividualTimeouts),
    ?assertEqual([], GlobalTimeouts),
    ?assertEqual(maps:size(NthPrimes), NrOfPrimes),

    lists:foreach(
      fun ({Nth, Value}) ->
              {Nth, ExpectedValue} = lists:keyfind(Nth, 1, ExpectedNthPrimes),
              ?assertEqual(Value, ExpectedValue)
      end,
      maps:to_list(NthPrimes)).


-spec calculate_primes__old_interface__test() -> ok.
calculate_primes__old_interface__test() ->
    application:ensure_all_started(taskforce),

    NrOfPrimes = 200,
    ExpectedNthPrimes = [{Nth, find_nth_prime_recur(Nth)}
                         || Nth <- lists:seq(1, 200)],

    IndividualTimeoutT = 2000, % in miliseconds
    GlobalTimeoutT = 10000,    % in miliseconds
    MinionCount = 4,           % 4 workers
    Tasks =
        [taskforce:new_task(Nth, fun find_nth_prime/1, [Nth], IndividualTimeoutT)
         || Nth <- lists:seq(1, NrOfPrimes)],

    {NthPrimes, IndividualTimeouts, GlobalTimeouts} =
        taskforce:execute_tasks(Tasks, GlobalTimeoutT, MinionCount),

    ?assertEqual([], IndividualTimeouts),
    ?assertEqual([], GlobalTimeouts),
    ?assertEqual(length(NthPrimes), NrOfPrimes),

    lists:foreach(
      fun ({Nth, Value}) ->
              {Nth, ExpectedValue} = lists:keyfind(Nth, 1, ExpectedNthPrimes),
              ?assertEqual(Value, ExpectedValue)
      end,
      NthPrimes).

-spec find_nth_prime(non_neg_integer()) -> pos_integer().
find_nth_prime(Nth) ->
    Prime = find_nth_prime_recur(Nth),
    ?debugFmt("~pth prime = ~p", [Nth, Prime]),
    Prime.

-spec find_nth_prime_recur(non_neg_integer()) -> pos_integer().
find_nth_prime_recur(Nth) when Nth =:= 1 ->
    1;
find_nth_prime_recur(Nth) ->
    Prev = find_nth_prime_recur(Nth - 1),
    find_prime(Prev + 1).

-spec find_prime(pos_integer()) -> pos_integer().
find_prime(N) ->
    case is_prime(N) of
        true -> N;
        false -> find_prime(N + 1)
    end.

-spec is_prime(pos_integer()) -> boolean().
is_prime(N) ->
    Factors = factors(N),
    length(Factors) =:= 2.

-spec factors(pos_integer()) -> [pos_integer(), ...].
factors(N) ->
    lists:filter(
      fun (Factor) -> N rem Factor =:= 0 end,
      lists:seq(1, N)).

-endif.
