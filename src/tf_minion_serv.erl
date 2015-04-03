% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(tf_minion_serv).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([{start_link, 1}]).


-include("include/taskforce.hrl").

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

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(MasterPid) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MasterPid) ->
    gen_server:start_link(?MODULE, [MasterPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([MasterPid]) ->
    State0 = #minion_state{master_pid = MasterPid,
                           master_monitor = monitor(process, MasterPid)},
    gen_server:cast(self(), get_task),
    {ok, State0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, #minion_state{}=State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(get_task, #minion_state{ exec_state=idle }=State) ->
    MasterPid = State#minion_state.master_pid,
    case catch gen_server:call(MasterPid, consume_task) of
        {ok, Task} ->
            #tf_task{id=TaskId,
                     fun_ref=TaskFun,
                     args=TaskArgs,
                     timeout=Timeout }=Task,

            MinionPid = self(),
            TaskTag = make_ref(),
            TimeoutTimer = erlang:send_after(Timeout, MinionPid, {task_timeout, TaskTag}),
            HandlerFun = fun() ->
                    Result = (catch apply(TaskFun, TaskArgs)),
                    gen_server:cast(MinionPid, {{task_result, TaskTag}, Result})
            end,
            TaskPid = spawn(HandlerFun),

            TaskRef = #task_ref{id = TaskId,
                                pid = TaskPid,
                                timeout_timer = TimeoutTimer,
                                tag = TaskTag},
            NewExecState = {running, TaskRef},
            NewState = State#minion_state{ exec_state=NewExecState },
            {noreply, NewState};
        {error, no_more_tasks} ->
            {stop, normal, State};
        {'EXIT', _} ->
            {stop, normal, State}
    end;


handle_cast({{task_result, TaskTag}, TaskResult},
            #minion_state{ exec_state=({running, (#task_ref{ tag=TaskTag }=TaskRef)}) }=State)
->
    #task_ref{id = TaskId,
              timeout_timer = TimeoutTimer }=TaskRef,
    erlang:cancel_timer(TimeoutTimer),
    MasterPid = State#minion_state.master_pid,
    gen_server:cast(MasterPid, {{task_completed, TaskId}, TaskResult}),
    gen_server:cast(self(), get_task),
    NewState = State#minion_state{ exec_state=idle },
    {noreply, NewState};

handle_cast({{task_result, _TaskTag}, _TaskResult}, #minion_state{}=State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Reference, process, _Pid, _Reason}, #minion_state{ master_monitor=Reference }=State) ->
    {stop, normal, State};


handle_info({task_timeout, TaskTag},
            #minion_state{ exec_state={running, (#task_ref{ tag=TaskTag }=TaskRef)} }=State)
->
    MasterPid = State#minion_state.master_pid,
    TaskId = TaskRef#task_ref.id,
    gen_server:cast(MasterPid, {{task_timeout, TaskId}}),
    gen_server:cast(self(), get_task),
    NewState = State#minion_state{ exec_state=idle },
    {noreply, NewState};

handle_info({task_timeout, _TaskTag}, #minion_state{}=State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #minion_state{}=_State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
