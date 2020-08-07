-module(rclref_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rclref_sup:start_link() of
      {ok, Pid} ->
          ok = riak_core:register([{vnode_module, rclref_vnode}]),
          ok = riak_core_node_watcher:service_up(rclref, self()),
          ok = setup_http_api(),
          {ok, Pid};
      {error, Reason} ->
          {error, Reason}
    end.

stop(_State) ->
    ok.

setup_http_api() ->
    Dispatch = cowboy_router:compile([{'_', [{"/rclref/:key", rclref_http_handler, []}]}]),
    HttpPort = rclref_config:http_port(),
    HttpAcceptors = rclref_config:http_acceptors(),
    HttpMaxConnections = rclref_config:http_max_connections(),

    logger:info("Starting HTTP API at port ~p", [HttpPort]),

    {ok, _} =
        cowboy:start_clear(rclref_http_listener,
                           [{port, HttpPort},
                            {num_acceptors, HttpAcceptors},
                            {max_connections, HttpMaxConnections}],
                           #{env => #{dispatch => Dispatch}}),

    ok.
