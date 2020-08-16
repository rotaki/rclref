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
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

% rclref:list_unique_keys, rclref:list_all_keys, rclref:list_unique_objects, rclref:list_all_objects
coverage_call_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(21, 40)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(21, 40)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],

    % check empty
    {ok, []} = rpc:call(Node, rclref, list_unique_keys, []),

    % put 20 key values
    lists:foreach(fun (RObj) ->
                          {ok, _} = rpc:call(Node, rclref, put, [RObj])
                  end,
                  RObjs),

    % check listing of unique keys
    {ok, GotUniqueKeys} = rpc:call(Node, rclref, list_unique_keys, []),
    ?assertEqual({ok, lists:sort(Keys)}, {ok, lists:sort(GotUniqueKeys)}),

    % check listing of all keys
    {ok, GotAllKeys} = rpc:call(Node, rclref, list_all_keys, []),
    lists:foreach(fun (Key) ->
                          ?N =:= count_keys(Key, GotAllKeys)
                  end,
                  Keys),

    % check listing of unique RObjs
    {ok, GotUniqueRObjs} = rpc:call(Node, rclref, list_unique_objects, []),
    lists:foreach(fun (RObj) ->
                          count_objects(RObj, GotUniqueRObjs) >= 1
                  end,
                  RObjs),

    % check listing of all RObjs
    {ok, GotAllRObjs} = rpc:call(Node, rclref, list_all_objects, []),
    lists:foreach(fun (RObj) ->
                          ?N =:= count_objects(RObj, GotAllRObjs)
                  end,
                  RObjs),

    % delete RObjs
    lists:foreach(fun (Key) ->
                          {ok, _} = rpc:call(Node, rclref, delete, [Key]),
                          ok = rpc:call(Node, rclref, reap_tombs, [Key])
                  end,
                  Keys),

    % check empty
    {ok, []} = rpc:call(Node, rclref, list_unique_keys, []),
    ok.

% private
% count number of occurrences of key X in list Y
count_keys(X, Y) ->
    length([E || E <- Y, E =:= X]).

% private
% count number of occurrences of object (key, value) X in list Y
count_objects(X, Y) ->
    length([E
            || E <- Y,
               rclref_object:key(E) =:= rclref_object:key(X),
               rclref_object:value(E) =:= rclref_object:value(X)]).
