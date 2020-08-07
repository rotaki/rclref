-module(coverage_call_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([coverage_call_test/1]).

-define(N, rclref_config:n_val()).

all() ->
    [coverage_call_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1],
    Ports = [30100],
    Nodes = node_utils:set_up_nodes(?MODULE, Names, Ports),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.


coverage_call_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    % put 20 key values
    lists:foreach(fun (RObj) ->
                          ok = rpc:call(Node, rclref, put, [RObj])
                  end,
                  RObjs),
    % check listing of unique keys
    ?assertEqual({ok, lists:usort(Keys)}, rpc:call(Node, rclref, list_unique_keys, [])),

    % check listing of all keys
    {ok, GotKeys} = rpc:call(Node, rclref, list_all_keys, []),

    lists:foreach(fun (Key) ->
                          ?N =:= count_keys(Key, GotKeys)
                  end,
                  Keys),

    % check listing of all RObjs
    {ok, GotRObjs} = rpc:call(Node, rclref, list_all_objects, []),
    lists:foreach(fun (RObj) ->
                          ?N =:= count_objects(RObj, GotRObjs)
                  end,
                  RObjs),
    ok.


% count number of occurrences of key X in list Y
% private
count_keys(X, Y) ->
    length([E || E <- Y, E =:= X]).

% count number of occurrences of object (key, value) X in list Y
% private
count_objects(X, Y) ->
    length([E
            || E <- Y,
               rclref_object:key(E) =:= rclref_object:key(X),
               rclref_object:value(E) =:= rclref_object:value(X)]).

% private
has_same_keyvalue(RObj1, RObj2) ->
    ?assertEqual(rclref_object:key(RObj1), rclref_object:key(RObj2)),
    ?assertEqual(rclref_object:value(RObj1), rclref_object:value(RObj2)),
    true.
