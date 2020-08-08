-module(handoff_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([handoff_test/1]).

all() ->
    [handoff_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1, node2],
    Ports = [10100, 10110],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.


handoff_test(Config) ->
    Nodes = [Node1, Node2] = ?config(nodes, Config),
    ct:pal("Nodes ~p", [Nodes]),

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
    lists:foreach(fun ({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node2, rclref, get, [Key]),
                          true =
                              lists:all(fun (GotRObj) ->
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
    lists:foreach(fun ({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node2, rclref, get, [Key]),
                          true =
                              lists:all(fun (GotRObj) ->
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



