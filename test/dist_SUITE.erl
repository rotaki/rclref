-module(dist_SUITE).

-include_lib("common_test/include/ct.hrl").

% common test
-export([all/0, init_per_suite/1, end_per_suite/1]).

% tests
-export([dummy/1]).

all() ->
    [dummy].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rclref),
    ok = node_manager:set_up_nodes_common([]),
    Config.

end_per_suite(Config) ->
    ok = application:stop(rclref),
    Config.

dummy(_Name) ->
   1 = 1.
    
    



    





