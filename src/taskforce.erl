% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(taskforce).

-include("include/taskforce.hrl").

-export([task/3]).         -ignore_xref({task,3}).
-export([execute/1]).      -ignore_xref({execute,1}).
-export([execute/2]).      -ignore_xref({execute,2}).

-opaque task() :: tf_task().
-type tasks() :: #{ TaskId :: term() => Task :: task() }.

-ifndef(pre19).
-type task_settings() ::
    #{ timeout := pos_integer() }.
-else.
-type task_settings() ::
    #{ timeout => pos_integer() }.
-endif.

-type execution_options() ::
    #{ timeout => pos_integer(),
       max_workers => pos_integer() }.

-ifndef(pre19).
-type result() ::
    #{ completed := #{ TaskId :: term() => TaskResult :: term() },
       individual_timeouts := [TaskId :: term()],
       global_timeouts := [TaskId :: term()] }.
-else.
-type result() ::
    #{ completed => #{ TaskId :: term() => TaskResult :: term() },
       individual_timeouts => [TaskId :: term()],
       global_timeouts => [TaskId :: term()] }.
-endif.

-export_type([task/0]).
-export_type([tasks/0]).
-export_type([execution_options/0]).
-export_type([result/0]).

%%%%
%% deprecated
-export([new_task/4]).         -ignore_xref({new_task,4}).
-export([execute_tasks/1]).    -ignore_xref({execute_tasks,1}).
-export([execute_tasks/2]).    -ignore_xref({execute_tasks,2}).
-export([execute_tasks/3]).    -ignore_xref({execute_tasks,3}).

-deprecated([{new_task,4, next_major_release}]).
-deprecated([{execute_tasks,'_', next_major_release}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface

-spec task(Function, Args, TaskSettings) -> Task
        when Function :: fun(),
             Args :: [term()],
             TaskSettings :: task_settings(),
             Task :: task().

task(Function, Args, TaskSettings) ->
    #tf_task{ id = implicit_id,
              fun_ref = Function,
              args = Args,
              timeout = maps:get(timeout, TaskSettings) }.


-spec execute(Tasks) -> Result
        when Tasks :: tasks(),
             Result :: result().

execute(Tasks) ->
    execute(Tasks, #{}).


-spec execute(Tasks, ExecutionOptions) -> Result
        when Tasks :: tasks(),
             ExecutionOptions :: execution_options(),
             Result :: result().

execute(Tasks, ExecutionOptions) ->
    TaskList = task_list(Tasks),
    result( execute_(TaskList, ExecutionOptions) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Old interface (deprecated)

%% @hidden
-spec new_task(Id, FunRef, Args, Timeout) -> Task
        when Id :: term(),
             FunRef :: fun(),
             Args :: [term()],
             Timeout :: pos_integer(),
             Task :: tf_task().

new_task(Id, FunRef, Args, Timeout) ->
    #tf_task{id = Id,
             fun_ref = FunRef,
             args = Args,
             timeout = Timeout}.


%% @hidden
-spec execute_tasks(TaskList) -> OldStyleResult
        when TaskList :: [Task],
             OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
             Completed :: [{TaskId, Task}],
             IndividualTimeouts :: [TaskId],
             Timeouts :: [TaskId],
             Task :: [task()],
             TaskId :: term().

execute_tasks(TaskList) ->
    old_style_result( execute_(TaskList, #{}) ).


%% @hidden
-spec execute_tasks(TaskList, Timeout) -> OldStyleResult
        when TaskList :: [Task],
             Timeout :: pos_integer(),
             OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
             Completed :: [{TaskId, Task}],
             IndividualTimeouts :: [TaskId],
             Timeouts :: [TaskId],
             Task :: [task()],
             TaskId :: term().

execute_tasks(TaskList, Timeout) ->
    old_style_result( execute_(TaskList, #{ timeout => Timeout }) ).

%% @hidden
-spec execute_tasks(TaskList, Timeout, MaxWorkers) -> OldStyleResult
        when TaskList :: [Task],
             Timeout :: pos_integer(),
             MaxWorkers :: pos_integer(),
             OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
             Completed :: [{TaskId, TaskResult}],
             IndividualTimeouts :: [TaskId],
             Timeouts :: [TaskId],
             Task :: [task()],
             TaskId :: term(),
             TaskResult :: term().

execute_tasks(TaskList, Timeout, MaxWorkers) ->
    old_style_result(
      execute_(TaskList, #{ timeout => Timeout,
                            max_workers => MaxWorkers })).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal

shuffle_list(L) ->
    KeyedL = [{V, rand_small()} || V <- L],
    SortedKeyedL = lists:keysort(2, KeyedL),
    [V || {V, _K} <- SortedKeyedL].

-ifdef(pre18).
rand_small() ->
    random:uniform(1 bsl 26).
-else.
rand_small() ->
    rand:uniform(1 bsl 26).
-endif.


-spec task_list(Tasks) -> TaskList
        when Tasks :: #{ TaskId => Task },
             TaskList :: [Task],
             TaskId :: term(),
             Task :: task().

task_list(Tasks) ->
    [Task#tf_task{ id = Id }
     || {Id, Task} <- maps:to_list(Tasks)].


-spec max_workers(ExecutionOptions) -> MaxWorkers
        when ExecutionOptions :: execution_options(),
             MaxWorkers :: pos_integer().

max_workers(ExecutionOptions) ->
    case maps:find(max_workers, ExecutionOptions) of
        {ok, MaxWorkers} ->
            MaxWorkers;
        error ->
            erlang:system_info(schedulers_online)
    end.


-spec global_timeout(TaskList, MaxWorkers, ExecutionOptions) -> GlobalTimeout
        when TaskList :: [Task],
             MaxWorkers :: pos_integer(),
             ExecutionOptions :: execution_options(),
             GlobalTimeout :: pos_integer(),
             Task :: task().

global_timeout(TaskList, MaxWorkers, ExecutionOptions) ->
    case maps:find(timeout, ExecutionOptions) of
        {ok, GlobalTimeout} ->
            GlobalTimeout;
        error ->
            lists:sum([Task#tf_task.timeout || Task <- TaskList]) div MaxWorkers
    end.


-spec execute_(TaskList, ExecutionOptions) -> BiddingResult
        when TaskList :: [task()],
             ExecutionOptions :: execution_options(),
             BiddingResult :: tf_bidding_result().

execute_(TaskList, ExecutionOptions) ->
    MaxWorkers = max_workers(ExecutionOptions),
    GlobalTimeout = global_timeout(TaskList, MaxWorkers, ExecutionOptions),
    ShuffledTasks = shuffle_list(TaskList),
    Bidding = #tf_bidding{ tasks = ShuffledTasks, timeout = GlobalTimeout },

    {ok, MasterPid} =
        supervisor:start_child(tf_master_sup, [self(), MaxWorkers]),

    {ok, BiddingResult} =
        gen_server:call(MasterPid, {do_my_bidding, Bidding}, infinity),

    BiddingResult.


-spec result(BiddingResult) -> Result
        when BiddingResult :: tf_bidding_result(),
             Result :: result().

result(BiddingResult) ->
    #tf_bidding_result{
       completed = CompletedList,
       individual_timeouts = IndividualTimeouts,
       global_timeouts = Timeouts } = BiddingResult,

    #{ completed => maps:from_list(CompletedList),
       individual_timeouts => IndividualTimeouts,
       global_timeouts => Timeouts }.


-spec old_style_result(BiddingResult) -> OldStyleResult
        when BiddingResult :: tf_bidding_result(),
             OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
             Completed :: [{TaskId, TaskResult}],
             IndividualTimeouts :: [TaskId],
             Timeouts :: [TaskId],
             TaskId :: term(),
             TaskResult :: term().

old_style_result(BiddingResult) ->
    #tf_bidding_result{
       completed = CompletedList,
       individual_timeouts = IndividualTimeouts,
       global_timeouts = Timeouts } = BiddingResult,

    {CompletedList, IndividualTimeouts, Timeouts}.
