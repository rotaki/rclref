-module(node_join_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([node_join_test/1]).


all() ->
    [node_join_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1, node2, node3, node4],
    Ports = [10000, 10010, 10020, 10030],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.
    
% put and get in distributed nodes
node_join_test(Config) ->
    Names = ?config(names, Config),
    Nodes = [Node1, Node2, Node3, Node4] = ?config(nodes, Config),
    ct:pal("Nodes ~p", [Nodes]),

    % Node2, Node3, Node4 will join Node1
    ok = rclref_cluster_manager:add_nodes_to_cluster(Node1, [Node2, Node3, Node4]),

    % generate key, value e.g. key--Node1, value--Node1
    Keys = ["key--" ++ atom_to_list(Name) || Name <- Names],
    Values = ["value--" ++ atom_to_list(Name) || Name <- Names],

    % put Key-Values into nodes respectively
    % Node1 puts {key--Node1, value--Node1}, Node2 puts {key--Node2, value--Node2}, Node3...
    lists:foreach(fun({Node, {Key, Value}}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value])
                  end,
                 lists:zip(Nodes, lists:zip(Keys, Values))),
    % get Key-Values
    % Node1 gets key--Node4, Node2 gets key--Node3, Node3 ...
    NodeKeySets = lists:zip(Nodes, lists:reverse(lists:zip(Keys, Values))),
    lists:foreach(fun ({Node, {Key, Value}}) ->
                          {ok, GotValues} = rpc:call(Node, rclref_client, get, [Key]),
                          true = lists:all(fun(GotValue) -> Value =:= GotValue end, GotValues)
                  end,
                  NodeKeySets),
    ok.
