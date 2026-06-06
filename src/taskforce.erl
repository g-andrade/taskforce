-module(taskforce).

-ifdef(E48).
-moduledoc """
Parallelise arbitrary tasks in a controlled way.

`taskforce` runs a group of independent tasks concurrently, bounding both the
number of workers and how long execution may take, and gathers their results.

Build tasks with `task/3`, then run them with `execute/1` or `execute/2`. See
the [README](readme.html) for an overview and examples.
""".
-endif.

-include("taskforce.hrl").

-export([task/3]).
-ignore_xref({task, 3}).
-export([execute/1]).
-ignore_xref({execute, 1}).
-export([execute/2]).
-ignore_xref({execute, 2}).

-opaque task() :: tf_task().
-type tasks() :: #{TaskId :: term() => Task :: task()}.

-type task_settings() ::
    #{timeout := pos_integer()}.

-type execution_options() ::
    #{
        timeout => pos_integer(),
        max_workers => pos_integer()
    }.

-type result() ::
    #{
        completed := #{TaskId :: term() => TaskResult :: term()},
        individual_timeouts := [TaskId :: term()],
        global_timeouts := [TaskId :: term()]
    }.

-export_type([task/0]).
-export_type([tasks/0]).
-export_type([task_settings/0]).
-export_type([execution_options/0]).
-export_type([result/0]).

%%%%
%% deprecated
-export([new_task/4]).
-ignore_xref({new_task, 4}).
-export([execute_tasks/1]).
-ignore_xref({execute_tasks, 1}).
-export([execute_tasks/2]).
-ignore_xref({execute_tasks, 2}).
-export([execute_tasks/3]).
-ignore_xref({execute_tasks, 3}).

-deprecated([{new_task, 4, next_major_release}]).
-deprecated([{execute_tasks, '_', next_major_release}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface

-ifdef(E48).
-doc """
Create a task that runs `Function` applied to `Args`.

`TaskSettings` must contain a `timeout`, in milliseconds, bounding how long the
individual task may run before being given up on.
""".
-endif.
-spec task(Function, Args, TaskSettings) -> Task when
    Function :: fun(),
    Args :: [term()],
    TaskSettings :: task_settings(),
    Task :: task().

task(Function, Args, TaskSettings) ->
    #tf_task{
        id = implicit_id,
        fun_ref = Function,
        args = Args,
        timeout = maps:get(timeout, TaskSettings)
    }.

-ifdef(E48).
-doc """
Run `Tasks` concurrently with default execution options.

Equivalent to `execute(Tasks, #{})`.
""".
-endif.
-spec execute(Tasks) -> Result when
    Tasks :: tasks(),
    Result :: result().

execute(Tasks) ->
    execute(Tasks, #{}).

-ifdef(E48).
-doc """
Run `Tasks` concurrently, subject to `ExecutionOptions`.

`ExecutionOptions` may bound the global `timeout` (in milliseconds) and the
maximum number of concurrent workers (`max_workers`); both default sensibly when
omitted. Returns the tasks that `completed`, along with the ids of those that
hit an `individual_timeouts` or a `global_timeouts`.
""".
-endif.
-spec execute(Tasks, ExecutionOptions) -> Result when
    Tasks :: tasks(),
    ExecutionOptions :: execution_options(),
    Result :: result().

execute(Tasks, ExecutionOptions) ->
    TaskList = task_list(Tasks),
    result(do_execute(TaskList, ExecutionOptions)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Old interface (deprecated)

-ifdef(E48).
-doc false.
-endif.
-spec new_task(Id, FunRef, Args, Timeout) -> Task when
    Id :: term(),
    FunRef :: fun(),
    Args :: [term()],
    Timeout :: pos_integer(),
    Task :: task().

new_task(Id, FunRef, Args, Timeout) ->
    #tf_task{
        id = Id,
        fun_ref = FunRef,
        args = Args,
        timeout = Timeout
    }.

-ifdef(E48).
-doc false.
-endif.
-spec execute_tasks(TaskList) -> OldStyleResult when
    TaskList :: [Task],
    OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
    Completed :: [{TaskId, Task}],
    IndividualTimeouts :: [TaskId],
    Timeouts :: [TaskId],
    Task :: [task()],
    TaskId :: term().

execute_tasks(TaskList) ->
    old_style_result(do_execute(TaskList, #{})).

-ifdef(E48).
-doc false.
-endif.
-spec execute_tasks(TaskList, Timeout) -> OldStyleResult when
    TaskList :: [Task],
    Timeout :: pos_integer(),
    OldStyleResult :: {Completed, IndividualTimeouts, Timeouts},
    Completed :: [{TaskId, Task}],
    IndividualTimeouts :: [TaskId],
    Timeouts :: [TaskId],
    Task :: [task()],
    TaskId :: term().

execute_tasks(TaskList, Timeout) ->
    old_style_result(do_execute(TaskList, #{timeout => Timeout})).

-ifdef(E48).
-doc false.
-endif.
-spec execute_tasks(TaskList, Timeout, MaxWorkers) -> OldStyleResult when
    TaskList :: [Task],
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
        do_execute(TaskList, #{
            timeout => Timeout,
            max_workers => MaxWorkers
        })
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal

shuffle_list(L) ->
    KeyedL = [{V, rand_small()} || V <- L],
    SortedKeyedL = lists:keysort(2, KeyedL),
    [V || {V, _K} <- SortedKeyedL].

rand_small() ->
    rand:uniform(1 bsl 26).

-spec task_list(Tasks) -> TaskList when
    Tasks :: #{TaskId => Task},
    TaskList :: [Task],
    TaskId :: term(),
    Task :: task().

task_list(Tasks) ->
    [
        Task#tf_task{id = Id}
     || {Id, Task} <- maps:to_list(Tasks)
    ].

-spec max_workers(ExecutionOptions) -> MaxWorkers when
    ExecutionOptions :: execution_options(),
    MaxWorkers :: pos_integer().

max_workers(ExecutionOptions) ->
    case maps:find(max_workers, ExecutionOptions) of
        {ok, MaxWorkers} ->
            MaxWorkers;
        error ->
            erlang:system_info(schedulers_online)
    end.

-spec global_timeout(TaskList, MaxWorkers, ExecutionOptions) -> GlobalTimeout when
    TaskList :: [Task],
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

-spec do_execute(TaskList, ExecutionOptions) -> BiddingResult when
    TaskList :: [task()],
    ExecutionOptions :: execution_options(),
    BiddingResult :: tf_bidding_result().

do_execute(TaskList, ExecutionOptions) ->
    MaxWorkers = max_workers(ExecutionOptions),
    GlobalTimeout = global_timeout(TaskList, MaxWorkers, ExecutionOptions),
    ShuffledTasks = shuffle_list(TaskList),
    Bidding = #tf_bidding{tasks = ShuffledTasks, timeout = GlobalTimeout},

    {ok, MasterPid} =
        supervisor:start_child(tf_master_sup, [self(), MaxWorkers]),

    {ok, BiddingResult} =
        gen_server:call(MasterPid, {do_my_bidding, Bidding}, infinity),

    BiddingResult.

-spec result(BiddingResult) -> Result when
    BiddingResult :: tf_bidding_result(),
    Result :: result().

result(BiddingResult) ->
    #tf_bidding_result{
        completed = CompletedList,
        individual_timeouts = IndividualTimeouts,
        global_timeouts = Timeouts
    } = BiddingResult,

    #{
        completed => maps:from_list(CompletedList),
        individual_timeouts => IndividualTimeouts,
        global_timeouts => Timeouts
    }.

-spec old_style_result(BiddingResult) -> OldStyleResult when
    BiddingResult :: tf_bidding_result(),
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
        global_timeouts = Timeouts
    } = BiddingResult,

    {CompletedList, IndividualTimeouts, Timeouts}.
