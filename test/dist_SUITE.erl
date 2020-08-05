-module(dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([node_join_test/1, handoff_test/1]).

all() ->
    [handoff_test, node_join_test].

init_per_suite(Config) ->
    % start rclref in masternode
    application:ensure_all_started(rclref),
    [{names, []}, {nodes, []} | Config].


init_per_testcase(node_join_test, Config) ->
    Names = [node1, node2, node3, node4],
    % start Nodes
    NodesWithStatus = node_utils:pmap(fun (Name) ->
                                              node_utils:start_node(Name, [])
                                      end,
                                      Names),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    Config0 = lists:keyreplace(names, 1, Config, {names, Names}),
    Config1 = lists:keyreplace(nodes, 1, Config0, {nodes, Nodes}),
    Config1;
init_per_testcase(handoff_test, Config) ->
    Names = [node1, node2],
    % start Nodes
    NodesWithStatus = node_utils:pmap(fun (Name) ->
                                              node_utils:start_node(Name, [])
                                      end,
                                      Names),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    Config0 = lists:keyreplace(names, 1, Config, {names, Names}),
    Config1 = lists:keyreplace(nodes, 1, Config0, {nodes, Nodes}),
    Config1.

end_per_suite(Config) ->
    % stop application in master node
    application:stop(rclref),
    Config.

end_per_testcase(_, Config) ->
    % kill nodes
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.


% put and get in distributed nodes
node_join_test(Config) ->
    Names = ?config(names, Config),
    Nodes = [Node1, Node2, Node3, Node4] = ?config(nodes, Config),
    ct:pal("Nodes ~p", [Nodes]),

    % construct cluster
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
    GotRObjs = lists:map(fun ({Node, Key}) ->
                                 {ok, RObjsPerNode} = rpc:call(Node, rclref, get, [Key]),
                                 lists:nth(1, RObjsPerNode)
                         end,
                         NodeKeySets),
    GotValues = [rclref_object:value(RObj) || RObj <- GotRObjs],
    ?assertEqual(lists:sort(Values), lists:sort(GotValues)),
    ok.

% put data in one node, then add another node, then the first node leave the cluster
% see if the data has moved to the second node
handoff_test(Config) ->
    Nodes = [Node1, Node2] = ?config(nodes, Config),
    ct:pal("Nodes ~p",[Nodes]),

    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],

    % put 20 key values into Node1
    lists:foreach(fun (RObj) ->
                          ?assertEqual(ok, rpc:call(Node1, rclref, put, [RObj]))
                  end,
                  RObjs),
    
    
    % join Node2 into Node1
    ok = rclref_cluster_manager:add_nodes_to_cluster(Node1, [Node2]),

    % check ring memebers after join
    CurrentRingMembers0 = rclref_cluster_manager:ring_members(Node1),
    ct:pal("Ring members after node joined :~p", [CurrentRingMembers0]),
    [Node1, Node2] = lists:sort(CurrentRingMembers0),
    
    % confirm whether 20 key values are reachable from Node2
    lists:foreach(fun({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node2, rclref, get, [Key]),
                          true = lists:all(fun (GotRObj) ->
                                                   has_same_keyvalue(RObj, GotRObj)
                                           end,
                                          GotRObjs)
                  end,
                  lists:zip(RObjs, Keys)),

    % Node1 leave the cluster
    ok = rclref_cluster_manager:leave_cluster(Node1),

    % wait until Node2 cannnot connect to Node1 anymore
    ok = time_utils:wait_until_disconnected(Node2, Node1),

    % check ring members after leave
    CurrentRingMembers1 = rclref_cluster_manager:ring_members(Node2),
    ct:pal("Ring members after node left :~p", [CurrentRingMembers1]),
    [Node2] = lists:sort(CurrentRingMembers1),

    % confirm whether 20 key values are still reachable from Node2
    lists:foreach(fun({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node2, rclref, get, [Key]),
                          true = lists:all(fun (GotRObj) ->
                                                   has_same_keyvalue(RObj, GotRObj)
                                           end,
                                          GotRObjs)
                  end,
                  lists:zip(RObjs, Keys)),

    ok.

% private
has_same_keyvalue(RObj1, RObj2) ->
    ?assertEqual(rclref_object:key(RObj1), rclref_object:key(RObj2)),
    ?assertEqual(rclref_object:value(RObj1), rclref_object:value(RObj2)),
    true.
