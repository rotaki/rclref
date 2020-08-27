# Vnodes

This page will explain how to implement the vnode module, [rclref_vnode.erl](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_vnode.erl) in detail.

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.

All commands requested by the user will eventually become commands to the vnode sent from the coordiantor. 
Commands to the vnode will be handled by `handle_command` function in the vnode module, which is  [rclref_vnode.erl](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_vnode.erl) in rclref. Let's look at how they are implemented for each request.

```plantuml
title rclref_client:get(Key)

[*] --> UserAPI 
UserAPI -down-> LowLevelAPI: get request
LowLevelAPI -down-> Supervisor
Supervisor -down-> Coordinator : simple one for one
Coordinator --> Vnode1
Coordinator --> Vnode2
Coordinator --> Vnode3
Vnode1 --> Coordinator
Vnode2 --> Coordinator
Vnode3 --> Coordinator
Vnode1 -down-> Backend1
Vnode2 -down-> Backend2
Vnode3 -down-> Backend3
Backend1 --> Vnode1
Backend2 --> Vnode2
Backend3 --> Vnode3
Coordinator -right-> LowLevelAPI
LowLevelAPI -up-> UserAPI

UserAPI: rclref_client.erl
LowLevelAPI: rclref.erl
Supervisor: rclref_get_statem_sup.erl
Coordinator: rclref_get_statem.erl
Vnode1: rclref_vnode.erl
Vnode2: rclref_vnode.erl
Vnode3: rclref_vnode.erl
Backend1: rclref_ets_backend.erl
Backend2: rclref_ets_backend.erl
Backend3: rclref_ets_backend.erl
Backend1: rclref_dets_backend.erl
Backend2: rclref_dets_backend.erl
Backend3: rclref_dets_backend.erl
```

## initializing

Before understanding how a vnode handles a request, let's look at how it initializes.

```erlang
%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    Mod =
        case rclref_config:storage_backend() of
          ets ->
              rclref_ets_backend;
          dets ->
              rclref_dets_backend;
          _ ->
              ?assert(false)
        end,
    {ok, ModState} = Mod:start(Partition, []),
    logger:debug("Successfully started ~p backend for partition ~p", [Mod, Partition]),
    State = #state{partition = Partition, mod = Mod, modstate = ModState},
    {ok, State}.
```

On initialization, it choose the backend module by reading the configuration and start a database (ets or dets) process.

## put

A put request from the client will be converted to `{kv_put_request, RObj, Pid, Node}` by the coordinator.
RObj stands for riak_object and this is a wrapper that wraps the basic components of the key-value object which is defined in [riak_object.erl](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_object.erl).

```erlang
-record(r_content, {value :: value(), vclock = vectorclock:new() :: vclock()}).
-record(r_object,
        {key :: key(),
         r_content :: #r_content{},
         partition :: non_neg_integer() | undefined,
         node :: node() | undefined}).
```

Pid is the process id of the put coordinator for the vnode to send back the results.
Node is the node of the client to update the vector clock.

```erlang
handle_command({kv_put_request, RObj, Pid, Node},
               _Sender,
               State0 = #state{partition = Partition, mod = Mod, modstate = ModState0}) ->
    Key = rclref_object:key(RObj),
    Value = rclref_object:value(RObj),

    % get will be issued before put
    % If a key is new to backend, store it with a new vector clock
    % If a key is not new to backend, store it with an updated vector clock
    % If get returns an error, put will be ignored
    case Mod:get(Key, ModState0) of
      {ok, not_found, ModState1} ->
          % Create content with a new vector clock
          VClock = rclref_object:new_vclock(),
          NewVClock = rclref_object:increment_vclock(Node, VClock),
          NewContent = rclref_object:new_content(Value, NewVClock),

          case Mod:put(Key, NewContent, ModState1) of
            {ok, ModState2} ->
                NewRObj = rclref_object:new(Key, NewContent, Partition, node()),
                rclref_put_statem:result_of_put(Pid, {ok, NewRObj}),
                State1 = State0#state{modstate = ModState2},
                {noreply, State1};
            {error, Reason, ModState2} ->
                logger:error("Failed to put kv with key: ~p, content: ~p for partition: ~p, "
                             "error: ~p",
                             [Key, NewContent, Partition, Reason]),
                VnodeError = rclref_object:new_error(Reason, Partition, node()),
                rclref_put_statem:result_of_put(Pid, {error, VnodeError}),
                State1 = State0#state{modstate = ModState2},
                {noreply, State1}
          end;
      {ok, Content, ModState1} ->
          % Create content with an updated vector clock
          VClock = rclref_object:vclock(Content),
          NewVClock = rclref_object:increment_vclock(Node, VClock),
          NewContent = rclref_object:new_content(Value, NewVClock),

          case Mod:put(Key, NewContent, ModState1) of
            {ok, ModState2} ->
                NewRObj = rclref_object:new(Key, NewContent, Partition, node()),
                rclref_put_statem:result_of_put(Pid, {ok, NewRObj}),
                State1 = State0#state{modstate = ModState2},
                {noreply, State1};
            {error, Reason, ModState2} ->
                logger:error("Failed to put kv with key: ~p, content: ~p for partition: ~p, "
                             "error: ~p",
                             [Key, NewContent, Partition, Reason]),
                VnodeError = rclref_object:new_error(Reason, Partition, node()),
                rclref_put_statem:result_of_put(Pid, {error, VnodeError}),
                State1 = State0#state{modstate = ModState2},
                {noreply, State1}
          end;
      {error, Reason, ModState1} ->
          logger:error("Failed to get kv (before put) with key: ~p for partition: ~p, "
                       "error: ~p",
                       [Key, Partition, Reason]),
          VnodeError = rclref_object:new_error(Reason, Partition, node()),
          rclref_put_statem:result_of_put(Pid, {error, VnodeError}),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1}
    end;
```

As commneted in the snippet above, a **get** to the database will be executed before the put request to see if the key is new or not. If the key is not new, it will have to update the vectorclock of the object and put the new value. If the key is new, it will just put the value.

If the put succeed, it will call `rclref_put_statem:result_of_put(Pid, {ok, NewRObj})` to notify the put coordinator that put has succeeded.
If the put errored, it will call `rclref_put_statem:result_of_put(Pid, {error, VnodeError})` to nofity the put coordinator that put has failed.


## get

A get request is converted to `{kv_get_request, Key, Pid}`. Pid is the process id of the get coordinator.

```erlang
handle_command({kv_get_request, Key, Pid},
               _Sender,
               State0 = #state{partition = Partition, mod = Mod, modstate = ModState0}) ->
    case Mod:get(Key, ModState0) of
      {ok, not_found, ModState1} ->
          VnodeError = rclref_object:new_error(not_found, Partition, node()),
          ok = rclref_get_statem:result_of_get(Pid, {error, VnodeError}),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1};
      {ok, Content, ModState1} ->
          RObj = rclref_object:new(Key, Content, Partition, node()),
          ok = rclref_get_statem:result_of_get(Pid, {ok, RObj}),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1};
      {error, Reason, ModState1} ->
          logger:error("Failed to get kv with key: ~p for partition: ~p, error: ~p",
                       [Key, Partition, Reason]),
          VnodeError = rclref_object:new_error(Reason, Partition, node()),
          rclref_get_statem:result_of_get(Pid, {error, VnodeError}),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1}
    end;
```

It will simply get the Key from the backend and return the results to the get coordinator.

If the get succeeded, `rclref_get_statem:result_of_get(Pid, {ok, RObj})` will be called.
If the get errored or the key was not_found, `rclref_get_statem:result_of_get(Pid, {error, VnodeError})` will be called.


## delete

A delete request form the client is acutally converted to a put request with Key=Key, Value=undefined by the LowLevelAPI ([rclref.erl](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref.erl)).
This means that the key is not completely deleted from the backend after the delete request. The reason why it is implemeted this way is to have better fault tolerancy. See the vector clock section in the [other](other.md)' page.
A key-value with undefined value is called a tombstone.

## handoff

Handoff is a mechanism used when riak_core_lite decides to relocate a vnode to another node. There are two major types of handoffs depending on the context.

- Hinted Handoff  
To ensure high availability of the database, riak-core allows writing even when the primary node responsible for the write is down by making another node take over the responsibility. This node is called secondary node or fallback node. When the primary node returns to service, the vnodes in the secondary node will pass back the data it has to the vnodes in the primary node. This exchange of data is called hinted handoff.

- Ownership Handoff  
An ownership handoff happens when a cluster member joins or leaves the cluster. Changes to the cluster will cause reassignment of vnodes to nodes which will trigger the handoff of data. This handoff is called ownership handoff.

Implementing the following functions in the vnode module is required in order to activate handoff.

```erlang
handle_handoff_command(#riak_core_fold_req_v2{foldfun = FoldFun, acc0 = Acc0},
                       _Sender,
                       State = #state{mod = Mod, modstate = ModState}) ->
    % FoldFun
    % -type fold_objects_fun() :: fun((term(), term(), any()) -> any() | no_return()).
    Acc = Mod:fold_objects(FoldFun, Acc0, [], ModState),
    {reply, Acc, State};
handle_handoff_command(Message, _Sender, State) ->
    logger:warning("handoff command ~p, ignoring", [Message]),
    {noreply, State}.

handoff_starting(TargetNode, State = #state{partition = Partition}) ->
    logger:info("handoff starting ~p: ~p", [Partition, TargetNode]),
    {true, State}.

handoff_cancelled(State = #state{partition = Partition}) ->
    logger:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(TargetNode, State = #state{partition = Partition}) ->
    logger:info("handoff finished ~p: ~p", [Partition, TargetNode]),
    {ok, State}.

handle_handoff_data(BinData,
                    State0 = #state{partition = Partition, mod = Mod, modstate = ModState0}) ->
    {Key, Content} = binary_to_term(BinData),
    logger:info("handoff data received ~p: ~p", [Partition, Key]),
    {ok, ModState1} = Mod:put(Key, Content, ModState0),
    State1 = State0#state{modstate = ModState1},
    {reply, ok, State1}.

encode_handoff_item(Key, Content) ->
    term_to_binary({Key, Content}).

handle_overload_command(_, _, _) ->
    ok.

handle_overload_info(_, _Idx) ->
    ok.

is_empty(State = #state{mod = Mod, modstate = ModState}) ->
    case Mod:is_empty(ModState) of
      true ->
          logger:info("is_empty: ~p", [true]),
          {true, State};
      false ->
          logger:info("is_empty: ~p", [false]),
          {false, State};
      Other ->
          logger:error("is_empty error reason :~p", [Other]),
          {false, State}
    end.

delete(State0 = #state{partition = Partition, mod = Mod, modstate = ModState0}) ->
    logger:info("delete partition: ~p", [Partition]),
    {ok, ModState1} = Mod:drop(ModState0),
    ok = Mod:stop(ModState1),
    State1 = State0#state{modstate = ModState1},
    {ok, State1}.
```

## coverage

Coverage calls are used for operations that involve the entire key space. For such operations, it is necessary to gather responses from all the vnodes. For example, in order to obtain a list of keys in the database, it needs to ask all the vnodes because keys are distributed around the cluster. 4 types coverage calls are supported in rclref in the LowLevelAPI.

- list_unique_keys
- list_all_keys
- list_unique_objects
- list_all_objects

In order to implement coverage calls, what we need to do first is to migrate [`riak_core_coverage_fsm.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/riak_core_coverage_fsm.erl) and [`riak_core_coverage_plan`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/riak_core_coverage_plan.erl) to our repository is needed at the moment. These modules come from the `riak_core` project because they are currently omitted from `riak_core_lite`. They are planned to be added to the `riak_core_lite_util` repository after updating the fsm to gen_statem.

The handler for coverage calls are implemented in the following way.

```erlang
handle_coverage({_, keys},
                _KeySpaces,
                {_, ReqId, _},
                State0 = #state{partition = _Partition, mod = Mod, modstate = ModState0}) ->
    Acc0 = [],
    Fun =
        fun (Key, Accum) ->
                [Key] ++ Accum
        end,
    Acc1 = Mod:fold_keys(Fun, Acc0, ModState0),
    {reply, {ReqId, Acc1}, State0};
handle_coverage({_, objects},
                _KeySpaces,
                {_, ReqId, _},
                State0 = #state{partition = Partition, mod = Mod, modstate = ModState0}) ->
    Acc0 = [],
    Fun =
        fun (Key, Content, Accum) ->
                [rclref_object:new(Key, Content, Partition, node())] ++ Accum
        end,
    Acc1 = Mod:fold_objects(Fun, Acc0, ModState0),
    {reply, {ReqId, Acc1}, State0}.
```
