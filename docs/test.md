path: docs
source: test.md

# How to test distributed application in Erlang?

In rclref, Common Test (CT) is used for integrated testing.
This post explains how to use CT for distributed applications assuming that the reader already knows how to use CT in a single node environment. The issue in testing distributed application in Erlang often relies on how to spawn multiple Erlang nodes from CT and how to manage their logs. The Goal of this post is to be able to test distirbuted Erlang application using `rebar3` and understand how to organize the logs of several distributed nodes.

The following is a snippet from [client_SUITE.erl](https://github.com/wattlebirdaz/rclref/blob/master/test/single_node/client_SUITE.erl) in rclref repository.


```erlang
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
```

The `init_per_suite/1` will start an applicaiotn with nodename "node1" on port 30400 by calling `node_utils:set_upnodes(Names, Ports, [{module, ?MODULE}])`. Internally, the node_util module basically spawns a slave node with `ct_slave:start/2` and configures environmental variales. CT will automatically call `put_get_delete_test/1` after `init_per_suite/1` is called.
`put_get_delete_test/1` is a basic test storing and deleteing 20 key-values to the slave node that has just been staretd by `init_per_suite/1`. Note that in order to access the remote node, remote procedure call should be used. For example, putting a key-value in the slave node is done by `ok = rpc:call(Node, rclref_client, put, [Key, Value])`.


Now let's see the [node_utls module](https://github.com/wattlebirdaz/rclref/blob/master/test/utils/node_utils.erl)  in more detail.

```erlang
-spec set_up_nodes([atom()], [non_neg_integer()], [tuple()]) -> [node()].
set_up_nodes(Names, Ports, Config) ->
    NodesWithStatus =
        node_utils:pmap(fun ({Name, Port}) ->
                                node_utils:start_node(Name, Port, Config)
                        end,
                        lists:zip(Names, Ports)),
    Nodes = [Node || {connect, Node} <- NodesWithStatus],
    ok = riak_utils:wait_until_ring_converged(Nodes),
    Nodes.

```
As shown in the snippet above, a node is spawned by `node_utls:start_node(Name, Port, Config)`.
The function `node_utils:pmap` is an asynchronous map fuction which is used to start multiple slave nodes asynchronously.

```erlang
-spec start_node(atom(), non_neg_integer(), [tuple()]) ->
                    {connect, node()} | {ready, node()}.
start_node(Name, Port, Config) ->
    ct:log("Starting node ~p", [Name]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    {ok, Cwd} = file:get_cwd(),
    % RclrefFolder is .../rclref/_build/test
    _RclrefFolder = filename:dirname(filename:dirname(Cwd)),
    NodeConfig =
        [{init_timeout, 3000},
         {startup_timeout, 3000},
         {monitor_master, true},
         {startup_functions, [{code, set_path, [CodePath]}]}],

    case ct_slave:start(Name, NodeConfig) of
      {ok, Node} ->
          % Load application to allow configuring the environment before starting
          ok = rpc:call(Node, application, load, [riak_core]),
          ok = rpc:call(Node, application, load, [rclref]),
          % Get remote working dir of node
          % NodeWorkingDir is .../rclref/_build/test/logs/ct_run.test@127.0.0.1.2020-00-00_00.00.00
          {ok, NodeWorkingDir} = rpc:call(Node, file, get_cwd, []),
          SuiteName = proplists:get_value(module, Config, ''),
          % Data Dirs
          ok =
              rpc:call(Node,
                       application,
                       set_env,
                       [riak_core,
                        ring_state_dir,
                        filename:join([NodeWorkingDir, "suites", SuiteName, Node, "data/ring"])]),
          ok =
              rpc:call(Node,
                       application,
                       set_env,
                       [riak_core,
                        platform_data_dir,
                        filename:join([NodeWorkingDir, "suites", SuiteName, Node, "data"])]),
          ok =
              rpc:call(Node,
                       application,
                       set_env,
                       [riak_core,
                        schema_dirs,
                        filename:join([NodeWorkingDir, "suites", SuiteName, Node, "data"])]),

          % Set ports
          ok = rpc:call(Node, application, set_env, [riak_core, handoff_port, Port]),
          ok = rpc:call(Node, application, set_env, [rclref, http_port, Port + 1]),

          % Logging Configuration
          LogRoot = filename:join([NodeWorkingDir, "suites", SuiteName, Node, "logs"]),
          ok = rpc:call(Node, application, set_env, [rclref, logger, log_config(LogRoot)]),
          rpc:call(Node, logger, set_primary_config, [level, all]),
          rpc:call(Node, logger, add_handlers, [rclref]),
          % redirect slave logs to ct_master logs
          ok = rpc:call(Node, application, set_env, [rclref, ct_master, node()]),
          ConfLog =
              #{level => debug,
                formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
                config => #{type => standard_io}},
          _ =
              rpc:call(Node,
                       logger,
                       add_handler,
                       [rclref_redirect_ct, ct_redirect_handler, ConfLog]),

          % Configuration
          ok = rpc:call(Node, application, set_env, [riak_core, ring_creation_size, 8]),
          {ok, _} = rpc:call(Node, application, ensure_all_started, [riak_core]),
          {ok, _} = rpc:call(Node, application, ensure_all_started, [rclref]),

          ct:pal("Node ~p stated with (handoff) port ~p", [Node, Port]),
          {connect, Node};
      {error, already_started, Node} ->
          ct:log("Node ~p already started, reusing node", [Node]),
          {ready, Node};
      {error, Reason, Node} ->
          ct:pal("Error starting node ~p, reason ~p, will retry", [Node, Reason]),
          ct_slave:stop(Name),
          time_utils:wait_until_offline(Node),
          start_node(Name, Port, Config)
    end.
```

`start_node/3` is the main fuction for starting up the nodes.
As soon as the slave node has started by using `ct_slave:start/2`, it is loading the applicaiton by rpc calls, such as `ok = rpc:call(Node, application, load, [riak_core])` and `ok = rpc:call(Node, application, load, [rclref])`. Once these are called, you can update the environment vairbales from the default values with `rpc:call(Node, appcliation, set_env, [...])`. In rclref, setting environmental values such as where to store the riak_core related data and which port is used for handoff and http communication are managed this way. 

```erlang
          % Logging Configuration
          LogRoot = filename:join([NodeWorkingDir, "suites", SuiteName, Node, "logs"]),
          ok = rpc:call(Node, application, set_env, [rclref, logger, log_config(LogRoot)]),
```

The snippet above is for setting up the logging environment. `LogRoot` is the log directory for each node. In rclref, nodes are not reused for each test suite for saftey reasons so logs are seperated from each suite. For test suite that uses two nodes, the log directory looks like the following.

```shell
|-- handoff_SUITE
|   |-- node1@localhost
|   |   |-- data
|   |   |   `-- ring
|   |   |       |-- riak_core_ring.default.20200817183651
|   |   |       `-- riak_core_ring.default.20200817183720
|   |   `-- logs
|   |       |-- debug.log
|   |       |-- error.log
|   |       |-- info.log
|   |       |-- notice.log
|   |       `-- warning.log
|   `-- node2@localhost
|       |-- data
|       |   `-- ring
|       |       |-- riak_core_ring.default.20200817183650
|       |       |-- riak_core_ring.default.20200817183710
|       |       `-- riak_core_ring.default.20200817183720
|       `-- logs
|           |-- debug.log
|           |-- error.log
|           |-- info.log
|           |-- notice.log
|           `-- warning.log
```
