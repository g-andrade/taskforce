% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(tf_minion_serv).

-ifdef(E48).
-moduledoc false.
-endif.

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-ignore_xref([{start_link, 1}]).

-include("taskforce.hrl").

-record(task_ref, {
    id :: any(),
    pid :: pid(),
    timeout_timer :: reference(),
    tag :: reference()
}).
-type task_ref() :: #task_ref{}.

-type exec_state() :: idle | {running, task_ref()}.

-record(minion_state, {
    master_pid :: pid(),
    master_monitor :: reference(),
    exec_state = 'idle' :: exec_state()
}).
-type state() :: #minion_state{}.

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(pid()) -> {ok, pid()}.
start_link(MasterPid) ->
    gen_server:start_link(?MODULE, [MasterPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([pid(), ...]) -> {ok, state()}.
init([MasterPid]) ->
    State0 = #minion_state{
        master_pid = MasterPid,
        master_monitor = monitor(process, MasterPid)
    },
    gen_server:cast(self(), get_task),
    {ok, State0}.

-spec handle_call(term(), gen_server:from(), state()) -> {noreply, state()}.
handle_call(_Request, _From, #minion_state{} = State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast(get_task, #minion_state{exec_state = idle} = State) ->
    MasterPid = State#minion_state.master_pid,
    case catch gen_server:call(MasterPid, consume_task) of
        {ok, Task} ->
            #tf_task{
                id = TaskId,
                fun_ref = TaskFun,
                args = TaskArgs,
                timeout = Timeout
            } = Task,

            MinionPid = self(),
            TaskTag = make_ref(),
            TimeoutTimer = erlang:send_after(Timeout, MinionPid, {task_timeout, TaskTag}),
            HandlerFun = fun() ->
                Result = (catch apply(TaskFun, TaskArgs)),
                gen_server:cast(MinionPid, {{task_result, TaskTag}, Result})
            end,
            TaskPid = spawn(HandlerFun),

            TaskRef = #task_ref{
                id = TaskId,
                pid = TaskPid,
                timeout_timer = TimeoutTimer,
                tag = TaskTag
            },
            NewExecState = {running, TaskRef},
            NewState = State#minion_state{exec_state = NewExecState},
            {noreply, NewState};
        {error, no_more_tasks} ->
            {stop, normal, State};
        {'EXIT', _} ->
            {stop, normal, State}
    end;
handle_cast(
    {{task_result, TaskTag}, TaskResult},
    #minion_state{exec_state = ({running, (#task_ref{tag = TaskTag} = TaskRef)})} = State
) ->
    #task_ref{
        id = TaskId,
        timeout_timer = TimeoutTimer
    } = TaskRef,
    _ = erlang:cancel_timer(TimeoutTimer),
    MasterPid = State#minion_state.master_pid,
    gen_server:cast(MasterPid, {{task_completed, TaskId}, TaskResult}),
    gen_server:cast(self(), get_task),
    NewState = State#minion_state{exec_state = idle},
    {noreply, NewState};
handle_cast({{task_result, _TaskTag}, _TaskResult}, #minion_state{} = State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info(
    {'DOWN', Reference, process, _Pid, _Reason}, #minion_state{master_monitor = Reference} = State
) ->
    {stop, normal, State};
handle_info(
    {task_timeout, TaskTag},
    #minion_state{exec_state = {running, (#task_ref{tag = TaskTag} = TaskRef)}} = State
) ->
    MasterPid = State#minion_state.master_pid,
    TaskId = TaskRef#task_ref.id,
    gen_server:cast(MasterPid, {{task_timeout, TaskId}}),
    gen_server:cast(self(), get_task),
    NewState = State#minion_state{exec_state = idle},
    {noreply, NewState};
handle_info({task_timeout, _TaskTag}, #minion_state{} = State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #minion_state{} = _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
