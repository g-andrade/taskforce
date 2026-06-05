% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(tf_minion_sup).

-ifdef(E48).
-moduledoc false.
-endif.

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([{start_link, 0}]).

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
        strategy => simple_one_for_one,
        intensity => 60,
        period => 3600
    },
    ChildSpec = #{
        id => masters,
        start => {tf_minion_serv, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker
    },
    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
