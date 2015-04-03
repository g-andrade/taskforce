% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(tf_master_serv).

-behaviour(gen_server).

%% API functions
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([{start_link, 2}]).

-include("include/taskforce.hrl").

-type exec_state() :: idle | {running, Bidder::any()}.

-record(master_state, {
        patron_pid :: pid(),
        patron_monitor :: reference(),
        max_minion_count :: pos_integer(),
        exec_state = 'idle' :: exec_state(),
        tasks :: [tf_task()],
        consumed_task_ids = [] :: [any()],
        completed = [] :: [any()],
        timedout_task_ids = [] :: [any()]
        }).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PatronPid, MaxMinionCount) ->
    gen_server:start_link(?MODULE, [PatronPid, MaxMinionCount], []).

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
init([PatronPid, MaxMinionCount]) ->
    State0 = #master_state{patron_pid = PatronPid,
                           patron_monitor = monitor(process, PatronPid),
                           max_minion_count = MaxMinionCount},
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
handle_call({do_my_bidding, #tf_bidding{}=Bidding}, Bidder, #master_state{ exec_state=idle }=State) ->
    #tf_bidding{tasks=Tasks,
                timeout=BiddingTimeout }=Bidding,

    MaxMinionCount = State#master_state.max_minion_count,
    MinionCount = min(length(Tasks), MaxMinionCount),
    ok = spawn_minions(MinionCount),

    NewExecState = {running, Bidder},
    NewState = State#master_state{exec_state = NewExecState,
                                  tasks = Tasks},
    case length(Tasks) == 0 of
        true ->
            {stop, normal, NewState};
        false ->
            erlang:send_after(BiddingTimeout, self(), bidding_timeout),
            {noreply, NewState}
    end;


handle_call(consume_task, {_MinionPid, _},
            #master_state{ exec_state={running, _}, tasks=[Task | Remaining] }=State)
->
    NewState = State#master_state{tasks = Remaining,
                                  consumed_task_ids = [Task#tf_task.id
                                                       | State#master_state.consumed_task_ids]},
    {reply, {ok, Task}, NewState};

handle_call(consume_task, {_MinionPid, _}, #master_state{ exec_state={running, _}, tasks=[] }=State) ->
    {reply, {error, no_more_tasks}, State}.


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
handle_cast({{task_completed, TaskId}, TaskResult}, #master_state{ exec_state={running, _} }=State) ->
    PrevCompleted = State#master_state.completed,
    NewCompleted = [{TaskId, TaskResult} | PrevCompleted],
    NewState = State#master_state{completed = NewCompleted},

    ConsumedIds = NewState#master_state.consumed_task_ids,
    Tasks = NewState#master_state.tasks,
    Timeouts = NewState#master_state.timedout_task_ids,
    case {length(Tasks), length(NewCompleted) + length(Timeouts), length(ConsumedIds)} of
        {0, V, V} ->
            {stop, normal, NewState};
        _ ->
            {noreply, NewState}
    end;

handle_cast({{task_timeout, TaskId}}, #master_state{ exec_state={running, _} }=State) ->
    PrevTimeouts = State#master_state.timedout_task_ids,
    NewTimeouts = [TaskId | PrevTimeouts],
    NewState = State#master_state{timedout_task_ids = NewTimeouts},

    ConsumedIds = NewState#master_state.consumed_task_ids,
    Tasks = NewState#master_state.tasks,
    Completed = NewState#master_state.completed,
    case {length(Tasks), length(NewTimeouts) + length(Completed), length(ConsumedIds)} of
        {0, V, V} ->
            {stop, normal, NewState};
        _ ->
            {noreply, NewState}
    end.

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
handle_info({'DOWN', Reference, process, _Pid, _Reason}, #master_state{ patron_monitor=Reference }=State) ->
    {stop, {shutdown, patron_death}, State};

handle_info(bidding_timeout, #master_state{ exec_state={running, _} }=State) ->
    {stop, {shutdown, bidding_timeout}, State}.

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
terminate(_Reason, #master_state{ exec_state=idle }=_State) ->
    ok;

terminate(_Reason, #master_state{ exec_state={running, Bidder} }=State) ->
    #master_state{tasks=NeverConsumed,
                  consumed_task_ids=ConsumedIds,
                  completed=Completed}=State,

    NeverConsumedIds = [Task#tf_task.id || Task <- NeverConsumed],
    CompletedIds = [TaskId || {TaskId, _} <- Completed],
    IndividualTimeouts = (ConsumedIds -- CompletedIds),
    GlobalTimeouts = NeverConsumedIds,

    BiddingResults = #tf_bidding_result{completed = Completed,
                                        individual_timeouts = IndividualTimeouts,
                                        global_timeouts = GlobalTimeouts},
    gen_server:reply(Bidder, {ok, BiddingResults}),
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

spawn_minions(MaxMinionCount) ->
    Self = self(),
    lists:foreach(
        fun (_Id) ->
                {ok, _MinionPid} = supervisor:start_child(tf_minion_sup, [Self])
        end,
        lists:seq(1, MaxMinionCount)
    ).
