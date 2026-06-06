# taskforce

[![Hex.pm Version](https://img.shields.io/hexpm/v/taskforce.svg?style=flat)](https://hex.pm/packages/taskforce)
[![CI](https://github.com/g-andrade/taskforce/actions/workflows/ci.yml/badge.svg)](https://github.com/g-andrade/taskforce/actions/workflows/ci.yml)
[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-24%20to%2029-blue)](https://www.erlang.org)

`taskforce` allows you to parallelise arbitrary tasks in a controlled way.

It runs a group of independent tasks concurrently, bounding both the number of
workers and how long execution may take, and gathers their results.


---------


### Creating tasks


```erlang

Tasks = #{ some_task => taskforce:task(fun work/0, Args, #{ timeout => 2000 }),
           similar_task => taskforce:task(fun work/0, Args123, #{ timeout => 2500 }),
           other_task => taskforce:task(fun other_work/1, OtherArgs, #{ timeout => 500 }) }.

```


### Executing tasks


```erlang

#{ completed := Completed } = taskforce:execute(Tasks),
SomeTaskResult = maps:get(some_task, Completed),
SimilarTaskResult = maps:get(similar_task, Completed),
OtherTaskResult = maps:get(other_task, Completed).

```


### Finely tuning execution


```erlang

ExecutionOptions = #{ max_workers => 8, timeout => 5000 },
#{ completed := Completed } = taskforce:execute(Tasks, ExecutionOptions),
% ...

```


### Individual task timeouts


```erlang

% ...
#{ individual_timeouts := IndividualTimeouts } = taskforce:execute(Tasks),
(length(IndividualTimeouts) > 0
 andalso io:format("oh noes! tasks with ids ~p timed-out",
                   [IndividualTimeouts]))

```


### Global execution timeouts


```erlang

% ...
#{ global_timeouts := GlobalTimeouts } = taskforce:execute(Tasks),
(length(GlobalTimeouts) > 0
 andalso io:format("execution ran out of time; tasks with ids ~p timed-out",
                   [GlobalTimeouts]))

```


### Full example


```erlang

%
% Calculate first 200 prime numbers using 4 processes,
% with a timeout of 2s per individual calculation and 10s
% for the whole batch.
%
NrOfPrimes = 200,
Tasks =
    maps:from_list(
        [{Nth, taskforce:task(fun fancy_lib:find_nth_prime/1,
                              [Nth], #{ timeout => 2000 })}
         || Nth <- lists:seq(1, NrOfPrimes)]),

ExecutionOptions =
    #{ % default is the sum of all individual task timeouts
       timeout => 10000,

       % default is number of active schedulers
       max_workers => 4 },

#{ completed := NthPrimes } = taskforce:execute(Tasks, ExecutionOptions),
io:format("200th prime is: ~p~n", [maps:get(200, NthPrimes)]).

```
Also in `examples/`.

