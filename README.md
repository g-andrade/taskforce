

# taskforce #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.1.0

__Authors:__ Guilherme Andrade ([`g@gandrade.net`](mailto:g@gandrade.net)).

`taskforce` allows you to parallelise arbitrary tasks in a controlled way.


---------


```erlang

%
% Calculate first 200 prime numbers using 4 processes,
% with a timeout of 2s per individual calculation and 10s
% for the whole batch.
%
NrOfPrimes = 200,
IndividualTimeoutT = 2000, % in miliseconds
GlobalTimeoutT = 10000,    % in miliseconds
MinionCount = 4,           % 4 workers
Tasks =
    [taskforce:new_task({nth,Nth}, fun fancy_lib:find_nth_prime/1,
                        [Nth], IndividualTimeoutT)
     || Nth <- lists:seq(1, NrOfPrimes)],

{NthPrimes, IndividualTimeouts, GlobalTimeouts} =
    taskforce:execute_tasks(Tasks, GlobalTimeoutT, MinionCount),

io:format("200th prime is: ~p~n", [proplists:get_value({nth,200}, NthPrimes)]).

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/taskforce.md" class="module">taskforce</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/taskforce_app.md" class="module">taskforce_app</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/taskforce_sup.md" class="module">taskforce_sup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/tf_master_serv.md" class="module">tf_master_serv</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/tf_master_sup.md" class="module">tf_master_sup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/tf_minion_serv.md" class="module">tf_minion_serv</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/taskforce/blob/master/doc/tf_minion_sup.md" class="module">tf_minion_sup</a></td></tr></table>

