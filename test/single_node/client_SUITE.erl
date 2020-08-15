-module(client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete_test/1, list_keys_test/1]).

all() ->
    [put_get_delete_test, list_keys_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1],
    Ports = [30400],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

put_get_delete_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    % check not_found
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key])
                  end,
                  Keys),
    % put values
    lists:foreach(fun({Key, Value}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value])
                  end,
                  lists:zip(Keys, Values)),
    % confirm values
    lists:foreach(fun({Key, Value}) ->
                          {ok, GotValues} = rpc:call(Node, rclref_client, get, [Key]),
                          true = lists:all(fun(GotValue) -> Value =:= GotValue end, GotValues)
                  end,
                  lists:zip(Keys, Values)),
    % delete values
    lists:foreach(fun(Key) ->
                          ok = rpc:call(Node, rclref_client, delete, [Key])
                  end,
                  Keys),

    % check not_found
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key])
                  end,
                  Keys),
    ok.
                                           
% rclref_client:list_keys
list_keys_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(21, 40)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(21, 40)],

    % check empty
    {ok, []} = rpc:call(Node, rclref_client, list_keys, []),
    
    % store Key-Values
    lists:foreach(fun({Key, Value}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value])
                  end,
                  lists:zip(Keys, Values)),

    % confirm Key-Values
    {ok, GotKeys} = rpc:call(Node, rclref_client, list_keys, []),
    ?assertEqual({ok, lists:sort(Keys)}, {ok, lists:sort(GotKeys)}),

    % delete Key-Values
    lists:foreach(fun(Key) ->
                          ok = rpc:call(Node, rclref_client, delete, [Key])
                  end,
                  Keys),

    % check empty
    {ok, []} = rpc:call(Node, rclref_client, list_keys, []),
    ok.
