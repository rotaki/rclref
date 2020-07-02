-module(rclref_cluster_manager).

-export([leave_cluster/0, add_nodes_to_cluster/1]).
-export([join_new_nodes/1, plan_and_commit/1, wait_until_ring_no_pending_changes/0,
         wait_until_ring_ready/1]).

-spec leave_cluster() -> ok | {error, term()}.
leave_cluster() ->
    riak_core:leave().

-spec add_nodes_to_cluster([node()]) -> ok | {error, ring_not_ready}.
add_nodes_to_cluster(Nodes) ->
    case riak_core_ring:ring_ready() of
      true ->
          join_new_nodes(Nodes);
      _ ->
          {error, ring_not_ready}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                %      Internal Functions     %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec join_new_nodes([node()]) -> ok.
join_new_nodes(Nodes) ->
    {ok, CurrentRing} = riak_core_ring_manager:get_my_ring(),
    CurrentNodeMembers = riak_core_ring:all_members(CurrentRing),
    NewNodeMembers = [NewNode
                      || NewNode <- Nodes, not lists:member(NewNode, CurrentNodeMembers)],
    plan_and_commit(NewNodeMembers).

-spec plan_and_commit([node()]) -> ok.
plan_and_commit([]) ->
    logger:info("No new nodes added to the ring of ~p", [node()]);
plan_and_commit(NewNodeMembers) ->
    % check if node is reachable
    [pong = net_adm:ping(Node) || Node <- NewNodeMembers],
    % join ring
    [ok = rpc:call(Node, riak_core, staged_join, [node()]) || Node <- NewNodeMembers],
    % check if ring is ready
    [ok = wait_until_ring_ready(Node) || Node <- NewNodeMembers],

    {ok, Actions, Transitions} = riak_core_claimant:plan(),
    logger:debug("Actions planned: ~p", [Actions]),
    logger:debug("Ring transitions planned: ~p", [Transitions]),

    ok = riak_core_claimant:commit(),
    logger:notice("Ring committed and ring structure is changing, New ring members: ~p",
                  [NewNodeMembers]),

    wait_until_ring_ready(node()),
    wait_until_ring_no_pending_changes(),
    ok.

-spec wait_until_ring_no_pending_changes() -> ok.
wait_until_ring_no_pending_changes() ->
    {ok, CurrentRing} = riak_core_ring_manager:get_my_ring(),
    Nodes = riak_core_ring:all_members(CurrentRing),
    F = fun () ->
                _ = rpc:multicall(Nodes, riak_core_vnode_manager, force_handoffs, []),
                {Rings, BadNodes} = rpc:multicall(Nodes, riak_core_ring_manager, get_raw_ring, []),
                Changes = [[] =:= riak_core_ring:pending_changes(Ring) || {ok, Ring} <- Rings],
                BadNodes =:= [] andalso
                  length(Changes) =:= length(Nodes) andalso
                    lists:all(fun (T) ->
                                      T
                              end,
                              Changes)
        end,
    case F() of
      true ->
          ok;
      _ ->
          timer:sleep(500),
          wait_until_ring_no_pending_changes()
    end.

-spec wait_until_ring_ready(node()) -> ok.
wait_until_ring_ready(Node) ->
    Status = rpc:call(Node, riak_core_ring, ring_ready, []),
    case Status of
      true ->
          ok;
      false ->
          timer:sleep(100),
          wait_until_ring_ready(Node)
    end.
