

# taskforce #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.2.0

__Authors:__ Guilherme Andrade ([`g@gandrade.net`](mailto:g@gandrade.net)).

`taskforce` allows you to parallelise arbitrary tasks in a controlled way.


---------


### <a name="Creating_tasks">Creating tasks</a> ###


```erlang

Tasks = #{ some_task => taskforce:task(function work/0, Args, #{ timeout => 2000 }),
           similar_task => taskforce:task(function work/0, Args123, #{ timeout => 2500 }),
           other_task => taskforce:task(function other_work/0, OtherArgs, #{ timeout => 500 }) }.

```


### <a name="Executing_tasks">Executing tasks</a> ###


```erlang

#{ completed := Completed } = taskforce:execute(Tasks),
SomeTaskResult = maps:get(some_task, Completed),
SimilarTaskResult = maps:get(similar_task, Completed),
OtherTaskResult = maps:get(other_task, Completed).

```


### <a name="Finely_tuning_execution">Finely tuning execution</a> ###


```erlang

ExecutionOptions = #{ max_workers => 8, timeout => 5000 },
#{ completed := Completed } = taskforce:execute(Tasks, ExecutionOptions),
% ...

```


### <a name="Individual_task_timeouts">Individual task timeouts</a> ###


```erlang

% ...
#{ individual_timeouts := IndividualTimeouts } = taskforce:execute(Tasks),
(length(IndividualTimeouts) > 0
 andalso io:format("oh noes! tasks with ids ~p timed-out",
                   [IndividualTimeouts]))

```


### <a name="Global_execution_timeouts">Global execution timeouts</a> ###


```erlang

% ...
#{ global_timeouts := GlobalTimeouts } = taskforce:execute(Tasks),
(length(GlobalTimeouts) > 0
 andalso io:format("execution ran out of time; tasks with ids ~p timed-out",
                   [GlobalTimeouts]))

```


### <a name="Full_example">Full example</a> ###


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


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/taskforce.md" class="module">taskforce</a></td></tr></table>

