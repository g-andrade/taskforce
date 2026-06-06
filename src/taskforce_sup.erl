-module(taskforce_sup).

-ifdef(E48).
-moduledoc false.
-endif.

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildrenSpecs =
        [
            #{
                id => masters_sup,
                start => {tf_master_sup, start_link, []},
                shutdown => 5000,
                type => supervisor
            },
            #{
                id => minions_sup,
                start => {tf_minion_sup, start_link, []},
                shutdown => 5000,
                type => supervisor
            }
        ],
    {ok, {SupFlags, ChildrenSpecs}}.
