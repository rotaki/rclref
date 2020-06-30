-module(node_manager).

-export([start_node/2, kill_and_restart_nodes/2, kill_nodes/1, brutal_kill_nodes/1, restart_nodes/2]).
-export([get_node_name/1, pmap/2, set_up_nodes_common/1, log_config/1, web_ports/1]).


-spec start_node(atom(), tuple()) -> {connenct, node()} | {ready, node()}.
start_node(Name, Config) ->
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    ct:log("Starting node ~p", [Name]),
    
    NodeConfig = [
                 {init_timeout, 3000},
                 {startup_timeout, 3000},
                 {monitor_master, true},
                  {startup_functions, [{code, set_path, [CodePath]}]}],
    
    case ct_slave:start(Name, NodeConfig) of
        {ok, Node} ->
            % Load application to allow configuring the environment before starting
            ok = rpc:call(Node, application, load, [riak_core]),
            % Get remote working dir of node
            {ok, NodeWorkingDir} = rpc:call(Node, file, get_cwd, []),

            % Data Dirs
            ok = rpc:call(Node, application, set_env, [riak_core, ring_state_dir, filename:join([NodeWorkingDir, Node, "data", "ring_state_dir"])]),
            % ok = rpc:call(Node, applicaiton, set_env, [riak_core, platform_data_dir, filename:join([NodeWorkingDir, Node, "data", "platfrom_data_dir"])]),
            ok = rpc:call(Node, application, set_env, [riak_core, schema_dirs, filename:join([NodeWorkingDir, Node, "data", "schema_dirs"])]),
            
            % Set ports
            Port = web_ports(Name),
            ok = rpc:call(Node, application, set_env, [riak_core, handoff_port, Port]),
            
            % Logging Configuration
            LogRoot = filename:join([NodeWorkingDir, Node, "logs"]),
            ok = rpc:call(Node, application, set_env, [rclref, logger, log_config(LogRoot)]),
            rpc:call(Node, logger, set_primary_config, [level, all]),
            rpc:call(Node, logger, add_handlers, [rclref]),
            % redirect slave logs to ct_master logs
            ok = rpc:call(Node, application, set_env, [rclref, ct_master, node()]),
            ConfLog = #{level => debug, formatter => {logger_formatter, #{single_line => true, max_size => 2048}}, config => #{type => standard_io}},
            _ = rpc:call(Node, logger, add_handler, [rclref_redirect_ct, ct_redirect_handler, ConfLog]),
            
            % Configuration
            %% ok = rpc:call(Node, applicaiton, set_env, [riak_core, ring_creation_size, 4]),
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
            start_node(Name, Config)
    end.

-spec kill_and_restart_nodes([node()], [tuple()]) -> [node()].
kill_and_restart_nodes(NodeList, Config) ->
    NewNodeList = brutal_kill_nodes(NodeList),
    restart_nodes(NewNodeList, Config).

-spec kill_nodes([node()]) -> [node()].
kill_nodes(NodeList) ->
    lists:map(fun(Node) -> {ok, Name} = ct_slave:stop(get_node_name(Node())), Name end, NodeList).

-spec brutal_kill_nodes([node()]) -> [node()].
brutal_kill_nodes(NodeList) ->
    lists:map(fun(Node) ->
                      ct:pal("Killing node ~p",[Node]),
                      OSPidToKill = rpc:call(Node, os, getpid, []),
                      ct_slave:stop(get_node_name(Node)),
                      rpc:cast(Node, os, cmd, [io_lib:format("kill -15 ~s", [OSPidToKill])]),
                      Node
              end, NodeList).

-spec restart_nodes([node()], [tuple()]) -> [node()].
restart_nodes(NodeList, Config) ->
    pmap(fun(Node) ->
                 ct:pal("Restarting node ~p", [Node]),
                 ct:log("Starting and waiting unitil vnodes are restarted at node ~p", [Node]),
                 start_node(get_node_name(Node), Config),
                 ct:log("Waiting unitil ring converged @ ~p", [Node]),
                 riak_utils:wait_unitl_ring_converged([Node]),
                 ct:log("Waiting unitl ready @ ~p", [Node]),
                 time_utils:wait_until(Node, fun wait_init:check_ready/1),
                 Node
                     end, NodeList
                 ).

-spec get_node_name(node()) -> atom().
get_node_name(NodeAtom) ->
    Node = atom_to_list(NodeAtom),
    {match, [{Pos, _Len}]} = re:run(Node, "@"),
    list_to_atom(string:substr(Node, 1, Pos)).

% asynchronous map
-spec pmap(fun(), list()) -> list().
pmap(F, L) ->
    Parent = self(),
    lists:foldl(
     fun(X, N) -> spawn_link(fun() ->
                                     Parent ! {pmap, N, F(X)}
                             end), N+1
     end, 0, L),
    L2 = [receive {pmap, N, R} -> {N, R} end || _ <- L],
    {_, L3} = lists:unzip(lists:keysort(1, L2)),
    L3.


-spec set_up_nodes_common([tuple()]) -> ok.
set_up_nodes_common(Config) ->
    Nodes = [dev1, dev2, dev3, dev4],
    pmap(fun(N) -> start_node(N, Config) end, Nodes),
    ok.
    
log_config(LogDir) ->
    DebugConfig = #{level => debug,
        formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
        config => #{type => {file, filename:join(LogDir, "debug.log")}}},

    InfoConfig = #{level => info,
        formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
        config => #{type => {file, filename:join(LogDir, "info.log")}}},

    NoticeConfig = #{level => notice,
        formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
        config => #{type => {file, filename:join(LogDir, "notice.log")}}},

    WarningConfig = #{level => warning,
        formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
        config => #{type => {file, filename:join(LogDir, "warning.log")}}},

    ErrorConfig = #{level => error,
        formatter => {logger_formatter, #{single_line => true, max_size => 2048}},
        config => #{type => {file, filename:join(LogDir, "error.log")}}},

    [
        {handler, debug_rclref, logger_std_h, DebugConfig},
        {handler, info_rclref, logger_std_h, InfoConfig},
        {handler, notice_rclref, logger_std_h, NoticeConfig},
        {handler, warning_rclref, logger_std_h, WarningConfig},
        {handler, error_rclref, logger_std_h, ErrorConfig}
    ].


web_ports(Name) ->
    case Name of
        dev1 ->
            10015;
        dev2 ->
            10025;
        dev3 ->
            10035;
        dev4 ->
            10045
    end.

                
