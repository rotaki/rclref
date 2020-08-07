-module(kill_and_restart_nodes).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([kill_and_restart_nodes_test/1]).

all() ->
    [kill_and_restart_nodes_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1, node2, node3, node4],
    Ports = [50300, 50310, 50320, 50330],
    Nodes = node_utils:set_up_nodes(?MODULE, Names, Ports),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

kill_and_restart_nodes_test(Config) ->
    Nodes = ?config(nodes, Config),
    % kill and restart node
    NewNodes = node_utils:kill_and_restart_nodes(Nodes, []),
    % check alive
    [pong = net_adm:ping(Node) || Node <- NewNodes],
    % kill nodes again
    KilledNodes = node_utils:kill_nodes(Nodes),
    [pang = net_adm:ping(Node) || Node <- KilledNodes],
    ok.
