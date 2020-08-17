-module(client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete_test/1, put_get_delete_with_options_test/1, read_repair_test/1, list_keys_test/1]).

all() ->
    [put_get_delete_test, put_get_delete_with_options_test, read_repair_test, list_keys_test].


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
put_get_delete_with_options_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    % check not_found
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 3}]])
                  end,
                  Keys),
    % put values
    lists:foreach(fun({Key, Value}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value, [{n_val, 3}, {w_val, 3}]])
                  end,
                  lists:zip(Keys, Values)),
    % confirm values
    lists:foreach(fun({Key, Value}) ->
                          {ok, [Value]} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 1}]]),
                          {ok, [Value, Value, Value]} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 3}]]),
                          {ok, [Value]} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 1}, {r_val, 1}]]),
                          {ok, [Value]} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 1}]])
                  end,
                  lists:zip(Keys, Values)),
    % delete values
    lists:foreach(fun(Key) ->
                          ok = rpc:call(Node, rclref_client, delete, [Key, [{n_val, 3}, {w_val, 3}]])
                  end,
                  Keys),

    % check not_found
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 1}]]),
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 3}]]),
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 1}, {r_val, 1}]]),
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 3}, {r_val, 1}]])
                  end,
                  Keys),
    ok.

read_repair_test(Config) ->
    [Node] = ?config(nodes, Config),
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    % check not_found
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 5}]])
                  end,
                  Keys),

    % put values to 5 vnodes
    lists:foreach(fun({Key, Value}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value, [{n_val, 5}, {w_val, 5}]])
                  end,
                  lists:zip(Keys, Values)),

    % delete from 2 vnodes
    lists:foreach(fun(Key) ->
                          ok = rpc:call(Node, rclref_client, delete, [Key, [{n_val, 2}, {w_val, 2}]])
                  end,
                  Keys),

    % get from 5 vnodes
    % this returns {error, partial} or {error, not_found} and triggers read_repair
    lists:foreach(fun(Key) ->
                          {error, _} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 5}]])
                  end,
                  Keys),

    % read repair
    timer:sleep(2000),

    % confirm read_repair
    % this will return {error, not_found} due to the read repair
    lists:foreach(fun(Key) ->
                          {error, not_found} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 5}]])
                  end,
                  Keys),

    % put to 2 vnodes
    lists:foreach(fun({Key, Value}) ->
                          ok = rpc:call(Node, rclref_client, put, [Key, Value, [{n_val, 2}, {w_val, 2}]])
                  end,
                 lists:zip(Keys, Values)),
    
    % get from 5 vnodes
    % this will return {error, partial} or {error, not_found} and triggers read_repair
    lists:foreach(fun(Key) ->
                          {error, _} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 5}]])
                  end,
                  Keys),

    % read repair
    timer:sleep(2000),
    
    % get from 5 vnodes
    % values will be repaired
    lists:foreach(fun({Key, Value}) ->
                          {ok, [Value, Value, Value, Value, Value]} = rpc:call(Node, rclref_client, get, [Key,[{n_val, 5}, {r_val, 5}]])
                  end,
                  lists:zip(Keys, Values)),

    % delete from 2 vnodes
    lists:foreach(fun(Key) ->
                          ok = rpc:call(Node, rclref_client, delete, [Key, [{n_val, 2}, {w_val, 2}]])
                  end,
                  Keys),

    % get from 3 vnodes
    % this returns {ok, [Value, Value, Value]} and triggers read_repair
    lists:foreach(fun({Key, Value}) ->
                          {ok, [Value, Value, Value]} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 3}]])
                  end,
                  lists:zip(Keys, Values)),

    % read repair    
    timer:sleep(2000),

    % confirm read_repair
    lists:foreach(fun(Key) ->
                          {error, _} = rpc:call(Node, rclref_client, get, [Key, [{n_val, 5}, {r_val, 5}]])
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
