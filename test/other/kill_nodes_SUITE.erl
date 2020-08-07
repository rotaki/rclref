-module(kill_nodes_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([kill_nodes_test/1]).

all() ->
    [kill_nodes_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1, node2, node3, node4],
    Ports = [50000, 50010, 50020, 50030],
    Nodes = node_utils:set_up_nodes(?MODULE, Names, Ports),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

kill_nodes_test(Config) ->
    Nodes = ?config(nodes, Config),
    % kill nodes
    KilledNodes = node_utils:kill_nodes(Nodes),
    % check dead
    [pang = net_adm:ping(Node) || Node <- KilledNodes],
    ok.
