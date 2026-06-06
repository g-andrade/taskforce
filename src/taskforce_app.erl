-module(taskforce_app).

-ifdef(E48).
-moduledoc false.
-endif.

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_StartType, _StartArgs) ->
    taskforce_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
