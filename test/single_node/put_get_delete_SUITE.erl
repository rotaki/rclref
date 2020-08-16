-module(put_get_delete_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete_test/1]).

all() ->
    [put_get_delete_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1],
    Ports = [30000],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.                                           

% rclref:get, rclref:put, rclref:delete, rclref:reap_tombs
put_get_delete_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(20, 40)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(20, 40)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    
    % confirm not_found
    lists:foreach(fun (Key) ->
                          {{ok, []}, {error, VnodeErrors}} = rpc:call(Node, rclref, get, [Key]),
                          true = lists:all(fun(VnodeError) ->
                                                   not_found =:= rclref_object:error_reason(VnodeError)
                                           end, VnodeErrors)
                  end,
                  Keys),

    % put 20 key values
    lists:foreach(fun (RObj) ->
                          {ok, GotRObjs} = rpc:call(Node, rclref, put, [RObj]),
                          true = 
                              lists:all(fun (GotRObj) ->
                                                have_same_keyvalue(RObj, GotRObj)
                                        end,
                                        GotRObjs)
                  end,
                  RObjs),

    % confirm 20 key values
    lists:foreach(fun ({RObj, Key}) ->
                          {ok, GotRObjs} = rpc:call(Node, rclref, get, [Key]),
                          true =
                              lists:all(fun (GotRObj) ->
                                                have_same_keyvalue(RObj, GotRObj)
                                        end,
                                        GotRObjs)
                  end,
                  lists:zip(RObjs, Keys)),

    % delete 20 key values
    lists:foreach(fun (Key) ->
                          {ok, GotRObjs} = rpc:call(Node, rclref, delete, [Key]),
                          RObj = rclref_object:new(Key, undefined),
                          true =
                              lists:all(fun (GotRObj) ->
                                                have_same_keyvalue(RObj, GotRObj)
                                        end,
                                       GotRObjs)
                  end,
                  Keys),

    % confirm undefined values
    lists:foreach(fun(Key) ->
                          {ok, GotRObjs} = rpc:call(Node, rclref, get, [Key]),
                          RObj = rclref_object:new(Key, undefined),
                          true =
                              lists:all(fun(GotRObj) ->
                                                 have_same_keyvalue(RObj, GotRObj)
                                         end,
                                        GotRObjs)
                  end,
                  Keys),

    % reap tombs
    lists:foreach(fun (Key) ->
                          ok = rpc:call(Node, rclref, reap_tombs, [Key])
                  end,
                  Keys),

    % confirm deleted
    lists:foreach(fun (Key) ->
                          {{ok, []}, {error, VnodeErrors}} = rpc:call(Node, rclref, get, [Key]),
                          true = lists:all(fun(VnodeError) ->
                                                   not_found =:= rclref_object:error_reason(VnodeError)
                                           end, VnodeErrors)
                  end,
                  Keys),
    ok.

% private
have_same_keyvalue(RObj1, RObj2) ->
    ?assertEqual(rclref_object:key(RObj1), rclref_object:key(RObj2)),
    ?assertEqual(rclref_object:value(RObj1), rclref_object:value(RObj2)),
    true.
