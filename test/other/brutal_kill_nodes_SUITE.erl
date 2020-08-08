-module(brutal_kill_nodes_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([brutal_kill_nodes_test/1]).

all() ->
    [brutal_kill_nodes_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1, node2, node3, node4],
    Ports = [50100, 50110, 50120, 50130],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

brutal_kill_nodes_test(Config) ->
    Nodes = ?config(nodes, Config),
    % brutal kill nodes
    KilledNodes = node_utils:brutal_kill_nodes(Nodes),
    % check dead
    [pang = net_adm:ping(Node) || Node <- KilledNodes],
    ok.
