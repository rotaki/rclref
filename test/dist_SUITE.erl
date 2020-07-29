-module(dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([node_join_test/1, handoff_test/1]).

all() ->
    [node_join_test, handoff_test].

init_per_suite(Config) ->
    % start application in master node
    application:ensure_all_started(rclref),
    Names = [node1, node2, node3, node4],

    % start Nodes
    NodesWithStatus = node_utils:pmap(fun (Name) ->
                                              node_utils:start_node(Name, [])
                                      end,
                                      Names),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    [{names, Names}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    % kill nodes
    Nodes = ?config(nodes, Config),
    node_utils:brutal_kill_nodes(Nodes),

    % stop application in master node
    application:stop(rclref),
    Config.

% test put and get in distributed nodes
% more will be added
node_join_test(Config) ->
    Names = ?config(names, Config),
    Nodes = ?config(nodes, Config),
    ct:pal("Nodes ~p", [Nodes]),

    % construct cluster
    ok = rclref_cluster_manager:add_nodes_to_cluster(Nodes),

    % generate key, value and create riak_object e.g. key--dev1, value--dev1
    Keys = ["key--" ++ atom_to_list(Name) || Name <- Names],
    Values = ["value--" ++ atom_to_list(Name) || Name <- Names],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],

    % put rclref_objects into nodes respectively
    % node dev1 puts {key--dev1, value--dev1}, dev2 puts {key--dev2, value--dev2}, dev3...
    lists:foreach(fun ({Node, RObj}) ->
                          ?assertEqual(ok, rpc:call(Node, rclref, put, [RObj]))
                  end,
                  lists:zip(Nodes, RObjs)),

    % get
    % node dev1 gets key--dev4, dev2 gets key--dev3, dev3 ...
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

% test handoffs
handoff_test(Config) ->
    Names = ?config(names, Config),
    Nodes = ?config(nodes, Config),

    % generate key, value and create riak_object e.g. key--dev1, value--dev1
    Keys = ["key--" ++ atom_to_list(Name) || Name <- Names],
    Values = ["value--" ++ atom_to_list(Name) || Name <- Names],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],

    % put
    lists:foreach(fun ({Node, RObj}) ->
                          ?assertEqual(ok, rpc:call(Node, rclref, put, [RObj]))
                  end,
                  lists:zip(Nodes, RObjs)),

    % construct cluster (This will trigger handoffs)
    ok = rclref_cluster_manager:add_nodes_to_cluster(Nodes),

    % get
    NodeKeySets = lists:zip(Nodes, lists:reverse(Keys)),
    GotRObjs = lists:map(fun ({Node, Key}) ->
                                 {ok, RObjsPerNode} = rpc:call(Node, rclref, get, [Key]),
                                 lists:nth(1, RObjsPerNode)
                         end,
                         NodeKeySets),
    GotValues = [rclref_object:value(RObj) || RObj <- GotRObjs],
    ?assertEqual(lists:sort(Values), lists:sort(GotValues)),
    ok.
