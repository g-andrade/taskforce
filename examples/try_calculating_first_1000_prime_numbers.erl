-module(try_calculating_first_1000_prime_numbers).
-export([run/0]).

run() ->
    NrOfPrimes = 1000,
    IndividualTimeoutT = 2000, % in miliseconds
    GlobalTimeoutT = 10000,    % in miliseconds
    MinionCount = 4,           % 4 workers
    Tasks =
        [taskforce:new_task(Nth, fun find_nth_prime/1, [Nth], IndividualTimeoutT)
         || Nth <- lists:seq(1, NrOfPrimes)],

    {_NthPrimes, _IndividualTimeouts, _GlobalTimeouts} =
        taskforce:execute_tasks(Tasks, GlobalTimeoutT, MinionCount).


% Brute force approach for demonstration purposes only

-spec find_nth_prime(non_neg_integer()) -> pos_integer().
find_nth_prime(Nth) when Nth =:= 1 ->
    1;
find_nth_prime(Nth) ->
    Prev = find_nth_prime(Nth - 1),
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
