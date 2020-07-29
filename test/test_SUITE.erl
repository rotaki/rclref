-module(test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([kill_nodes/1, brutal_kill_nodes/1, kill_and_restart_nodes/1]).

all() ->
    [kill_nodes, brutal_kill_nodes, kill_and_restart_nodes].

init_per_testcase(_, Config) ->
    Names = [node1, node2, node3, node4],
    % start Nodes
    NodesWithStatus = node_utils:pmap(fun (Name) ->
                                              node_utils:start_node(Name, [])
                                      end,
                                      Names),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    % check alive
    [pong = net_adm:ping(Node) || Node <- Nodes],
    [{nodes, Nodes} | Config].

end_per_testcase(_, Config) ->
    Config.

kill_nodes(Config) ->
    Nodes = ?config(nodes, Config),
    % kill nodes
    KilledNodes = node_utils:brutal_kill_nodes(Nodes),
    % check dead
    [pang = net_adm:ping(Node) || Node <- KilledNodes],
    ok.

brutal_kill_nodes(Config) ->
    Nodes = ?config(nodes, Config),
    % brutal kill nodes
    KilledNodes = node_utils:brutal_kill_nodes(Nodes),
    % check dead
    [pang = net_adm:ping(Node) || Node <- KilledNodes],
    ok.

kill_and_restart_nodes(Config) ->
    Nodes = ?config(nodes, Config),
    % kill and restart ndoes
    NewNodes = node_utils:kill_and_restart_nodes(Nodes, []),
    % check alive
    [pong = net_adm:ping(Node) || Node <- NewNodes],
    ok.
