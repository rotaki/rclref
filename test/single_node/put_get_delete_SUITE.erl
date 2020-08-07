-module(put_get_delete_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete_test/1]).

-define(N, rclref_config:n_val()).

all() ->
    [put_get_delete_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1],
    Ports = [30000],
    Nodes = node_utils:set_up_nodes(?MODULE, Names, Ports),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

put_get_delete_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    lists:foreach(fun (Key) ->
                          {error, not_found} = rpc:call(Node, rclref, get, [Key])
                  end,
                  Keys),
    % put 20 key values
    lists:foreach(fun (RObj) ->
                          ok = rpc:call(Node, rclref, put, [RObj])
                  end,
                  RObjs),
    % confirm 20 key values
    lists:foreach(fun ({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node, rclref, get, [Key]),
                          true =
                              lists:all(fun (GotRObj) ->
                                                has_same_keyvalue(RObj, GotRObj)
                                        end,
                                        GotRObjs)
                  end,
                  lists:zip(RObjs, Keys)),
    % delete 20 key values
    lists:foreach(fun (Key) ->
                          ok = rpc:call(Node, rclref, delete, [Key])
                  end,
                  Keys),
    % confirm deleted
    lists:foreach(fun (Key) ->
                          {error, not_found} = rpc:call(Node, rclref, get, [Key])
                  end,
                  Keys),
    ok.

% private
has_same_keyvalue(RObj1, RObj2) ->
    ?assertEqual(rclref_object:key(RObj1), rclref_object:key(RObj2)),
    ?assertEqual(rclref_object:value(RObj1), rclref_object:value(RObj2)),
    true.
