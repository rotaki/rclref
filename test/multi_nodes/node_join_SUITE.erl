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
    Nodes = node_utils:set_up_nodes(?MODULE, Names, Ports),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes} | Config].

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

    % generate key, value and create riak_object e.g. key--Node1, value--Node1
    Keys = ["key--" ++ atom_to_list(Name) || Name <- Names],
    Values = ["value--" ++ atom_to_list(Name) || Name <- Names],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],

    % put rclref_objects into nodes respectively
    % node Node1 puts {key--Node1, value--Node1}, Node2 puts {key--Node2, value--Node2}, Node3...
    lists:foreach(fun ({Node, RObj}) ->
                          ?assertEqual(ok, rpc:call(Node, rclref, put, [RObj]))
                  end,
                  lists:zip(Nodes, RObjs)),

    % get
    % Node1 gets key--Node4, Node2 gets key--Node3, Node3 ...
    % rclref:get will return several copies of the riak_objects depending on n_val
    % check the first copy only
    NodeKeySets = lists:zip(Nodes, lists:reverse(Keys)),
    GotRObjs =
        lists:map(fun ({Node, Key}) ->
                          {ok, RObjsPerNode} = rpc:call(Node, rclref, get, [Key]),
                          lists:nth(1, RObjsPerNode)
                  end,
                  NodeKeySets),
    GotValues = [rclref_object:value(RObj) || RObj <- GotRObjs],
    ?assertEqual(lists:sort(Values), lists:sort(GotValues)),
    ok.
