% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(taskforce).
-export([new_task/4,
         execute_tasks/1,
         execute_tasks/2,
         execute_tasks/3]).

-ignore_xref([{new_task, 4},
              {execute_tasks, 1},
              {execute_tasks, 2},
              {execute_tasks, 3}]).

%% @headerfile "../include/taskforce.hrl"
-include("include/taskforce.hrl").

-type execution_result() :: {Completed::tf_proplist(tf_task_id(), tf_task_result()),
                             IndividualTimeouts::[tf_task_id()],
                             GlobalTimeouts::[tf_task_id()]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new_task(Id::tf_task_id(), FunRef::tf_fun_ref(), Args::tf_fun_args(), Timeout::tf_timeout())
        -> tf_task().
new_task(Id, FunRef, Args, Timeout) ->
    #tf_task{id = Id,
             fun_ref = FunRef,
             args = Args,
             timeout = Timeout}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec execute_tasks(Tasks::[tf_task()]) -> execution_result().
execute_tasks(Tasks) ->
    SchedulerCount = erlang:system_info(schedulers_online),
    TotalTimeout = (lists:sum([Task#tf_task.timeout || Task <- Tasks]) div SchedulerCount),
    execute_tasks(Tasks, TotalTimeout, SchedulerCount).

-spec execute_tasks(Tasks::[tf_task()], TotalTimeout::tf_timeout()) -> execution_result().
execute_tasks(Tasks, TotalTimeout) ->
    SchedulerCount = erlang:system_info(schedulers_online),
    execute_tasks(Tasks, TotalTimeout, SchedulerCount).

-spec execute_tasks(Tasks::[tf_task()], TotalTimeout::tf_timeout(), MaxMinionCount::pos_integer())
        -> execution_result().
execute_tasks(Tasks, TotalTimeout, MaxMinionCount) ->
    {_, ShuffledTasks} = lists:unzip(
            lists:keysort(1, lists:zip([crypto:rand_uniform(0, 1 bsl 31)
                                        || _ <- lists:seq(1, length(Tasks))],
                                       Tasks)) ),
    Bidding = #tf_bidding{tasks = ShuffledTasks,
                          timeout = TotalTimeout},
    {ok, MasterPid} = supervisor:start_child(tf_master_sup, [self(), MaxMinionCount]),
    {ok, BiddingResult} = gen_server:call(MasterPid, {do_my_bidding, Bidding},
                                          infinity),
    {BiddingResult#tf_bidding_result.completed,
     BiddingResult#tf_bidding_result.individual_timeouts,
     BiddingResult#tf_bidding_result.global_timeouts}.
