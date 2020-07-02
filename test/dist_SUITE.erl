-module(dist_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_and_get/1]).


all() ->
    [put_and_get].

init_per_suite(Config) ->
    Names = [dev1, dev2, dev3, dev4],
    % Start Nodes
    NodesWithStatus = node_utils:pmap(fun(Name) -> node_utils:start_node(Name, []) end, Names),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
    % kill nodes
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

% test put and get in distributed nodes
% more will be added
put_and_get(Config) ->
    % normal put
    Nodes = ?config(nodes, Config),
    Keys = ["key--" ++ atom_to_list(Node) || Node <- Nodes],
    Values = ["value--" ++ atom_to_list(Node) || Node <- Nodes],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    [ok = rpc:call(Node, rclref, put, [RObj]) || {Node, RObj} <- lists:zip(Nodes, RObjs)],

    % normal get
    GotRObjs = [lists:nth(1, rpc:call(Node, rclref, get, [Key])) || {Node, Key} <- lists:zip(Nodes, Keys)],
    GotValues = [rclref_object:value(RObj) || RObj <- GotRObjs],
    % check values
    [Value = GotValue || {Value, GotValue} <- lists:zip(Values, GotValues)],
    ok.
