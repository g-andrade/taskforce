% vim: set expandtab softtabstop=4 shiftwidth=4:
%% @hidden
-module(tf_master_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([{start_link, 0}]).

-define(CHILD(Id, Mod, Type, Args, Restart), {Id, {Mod, start_link, Args},
                                              Restart, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ChildSpec = ?CHILD(masters, tf_master_serv, worker, [], temporary),
    {ok, {{simple_one_for_one, 60, 3600}, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
