# Coordinator

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.

This page provides an overview of how coordinators are implemented in rclref.

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

note right of Coordinator
    Calculate hash to distribute request to vnodes.
    When enough responses from vnodes are collected, return to API.
end note

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

## What does a coordinator do?

Applications that use distributed databases tend to store multiple versions of the same object because it results in better fault tolerance. In riak_core_lite, this is done by distributing the same request to multiple vnodes such as putting a key-value. When a client requests a key-value, even when the primary vnode responsible for the key-value is not responding due to a failure, it is possible to retrieve it from other nodes with the replicas. The coordinator is responsible for distributing the request to the vnodes and collecting the results from them.

## How many replicas does it make?
To understand the mechanism of replication we need to understand the N, R, and W values. These are numbers that defines the level of fault tolerancy and reliability of the operations.

- N  
  Number of how many replicas it will store in the backend.
- R  
  Number of how many responses the coordinator will receive from the vnodes on **get** before it will return the result to the client
- W  
  Number of how many responses the coordinator will receive from the vnodes on **put** before it will return the result to the client
  
For instance, if N=3, R=1, and W=2, this means whenever a client issues a request, these request will be sent to 3 vnodes by the coordinator. If it is a get request, the coordinator will wait for 1 vnode to respond with the data and send it back to the client. If it is a put request, the coordinator will wait for 2 vnodes to respond and send it back to the client.

If N is large, it results in better fault tolerance because it is likely that the copies be distributed to different nodes in the cluster by the consistent hashing algorithm. This, however, also means that it will take more time for a request to terminate.

If R is small, it results in better throughput for get request. This, however, means that the reliability of the data is comparatively low because it will only wait for a partial number of vnodes to respond.

If W is small, it results in better throughput for put request. This, however, means that the reliability of the put has succeeded is comparatively low because it will only wait for a partial number of vnodes to respond.


## Implementation details

There are 3 coordinators in rclref: `rclref_put_statem.erl`, `rclref_get_statem.erl`, `rclref_coverage_fsm`.

### put coordinator

A put coordinator is in [`rclref_put_statem.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_put_statem.erl).
This is implemented using gen_statem behaviour with 1 main state, which is waiting.

On initialize, it will send the put request to N vnodes.
Then in the waiting state, it will wait for either of the condition holds.

- W vnodes respond with ok
- N-W vnodes respond with errror


```erlang
% State function
% WAITING STATE will wait for the responses from vnodes until
% When a vnode return {ok, RObj}
waiting(cast,
        {ok, RObj},
        State =
            #state{req_id = ReqId,
                   client_pid = ClientPid,
                   w_val = W,
                   num_ok = NumOk0,
                   riak_objects = RObjs0}) ->
    % Update State
    RObjs = [RObj] ++ RObjs0,
    NumOk = NumOk0 + 1,
    NewState = State#state{num_ok = NumOk, riak_objects = RObjs},

    % When more than or equal to W vnodes responded with {ok, RObj}, return W RObjs to client
    case NumOk >= W of
      true ->
          ClientPid ! {ReqId, {ok, RObjs}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
% When a vnode return {error, VnodeError}
waiting(cast,
        {error, VnodeError},
        State =
            #state{req_id = ReqId,
                   client_pid = ClientPid,
                   n_val = N,
                   w_val = W,
                   num_vnode_error = NumVnodeError0,
                   vnode_errors = VnodeErrors0,
                   riak_objects = RObjs0}) ->
    % Update State
    NumVnodeError = NumVnodeError0 + 1,
    VnodeErrors = [VnodeError] ++ VnodeErrors0,
    NewState = State#state{num_vnode_error = NumVnodeError, vnode_errors = VnodeErrors},

    % When more than (N-W) vnodes responded with {error, VnodeError}, return all RObjs and VnodeErrors it has received to client
    case NumVnodeError > N - W of
      true ->
          ClientPid ! {ReqId, {{ok, RObjs0}, {error, VnodeErrors}}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(state_timeout,
        hard_stop,
        State = #state{req_id = ReqId, client_pid = ClientPid}) ->
    ClientPid ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.
```


### get coordinator

A get coordinator is in [`rclref_get_statem`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_get_statem.erl).
It is also implemented using gen_statem behaviour which has two main states, waiting and finalize.

On initialize, the get coordinator will send the get requests to N vnodes.
Then in the waiting state, it wait until either of the conditions holds.

- R vnodes respond with a valid value
- N-R vnodes respond with an error


After the waiting state, it will transit to the finalize state where it repairs the values of the vnodes with broken data.
In transit state, the get coordinator will first wait for N vnodes to respond and then compute the latest value using the vector clocks of the valid values it has received.
Then, it will send this latest value to the vnodes which did not respond with a valid value.
This is called **read_repair** and you can read more about it [here](https://docs.riak.com/riak/kv/2.2.3/learn/concepts/replication.1.html#read-repair).

```erlang
% FINALIZE STATE will wait for ?N vnodes to return response and then issue read_pair
% When a vnode returns {ok, RObj}
finalize(cast,
         {ok, RObj},
         State =
             #state{n_val = N,
                    num_ok = NumOk0,
                    num_vnode_error = NumVnodeError0,
                    riak_objects = RObjs0,
                    undefined_objects = UObjs0}) ->
    % Update State
    case rclref_object:value(RObj) of
      undefined ->
          NumVnodeError = NumVnodeError0 + 1,
          UObjs = UObjs0 ++ [RObj],
          NumOk = NumOk0,
          RObjs = RObjs0;
      _ ->
          NumVnodeError = NumVnodeError0,
          UObjs = UObjs0,
          NumOk = NumOk0 + 1,
          RObjs = RObjs0 ++ [RObj]
    end,

    NewState =
        State#state{num_ok = NumOk,
                    num_vnode_error = NumVnodeError,
                    riak_objects = RObjs,
                    undefined_objects = UObjs},

    % When all ?N vnodes has responded, do read_repair
    case NumOk + NumVnodeError >= N of
      true ->
          MergedRObj = rclref_object:merge(RObjs ++ UObjs),
          ok = repair(MergedRObj, N, RObjs ++ UObjs),
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
% When a vnode returns {error, VnodeError}
finalize(cast,
         {error, VnodeError},
         State =
             #state{n_val = N,
                    num_ok = NumOk0,
                    num_vnode_error = NumVnodeError0,
                    riak_objects = RObjs0,
                    undefined_objects = UObjs0,
                    vnode_errors = VnodeErrors0}) ->
    % Update Satte
    NumVnodeError = NumVnodeError0 + 1,
    VnodeErrors = VnodeErrors0 ++ [VnodeError],
    NewState = State#state{num_vnode_error = NumVnodeError, vnode_errors = VnodeErrors},

    % When all ?N vnodes has responded, do read_repair
    case NumOk0 + NumVnodeError >= N of
      true ->
          case RObjs0 ++ UObjs0 of
            % When any of the vnodes responded with {ok, RObj}, do not issue read_repair
            [] ->
                {stop, normal, NewState};
            _ ->
                MergedRObj = rclref_object:merge(RObjs0 ++ UObjs0),
                ok = repair(MergedRObj, N, RObjs0 ++ UObjs0),
                {stop, normal, NewState}
          end;
      false ->
          {keep_state, NewState}
    end;
% When finalize state timeouts, issue a read_repair
finalize(state_timeout,
         hard_stop,
         State = #state{n_val = N, riak_objects = RObjs, undefined_objects = UObjs}) ->
    case RObjs ++ UObjs of
      [] ->
          {stop, normal, State};
      _ ->
          MergedRObj = rclref_object:merge(RObjs ++ UObjs),
          ok = repair(MergedRObj, N, RObjs ++ UObjs),
          {stop, normal, State}
    end;
finalize(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.

% Use RObj to repair vnodes with different content (i.e Value, VClock)
repair(RObj, N, RObjs) ->
    Key = rclref_object:key(RObj),
    Content = rclref_object:content(RObj),
    % Exclude repairing of vnodes that has the same content
    OkNodesIndexes =
        [{rclref_object:partition(X), rclref_object:node(X)}
         || X <- RObjs, Key =:= rclref_object:key(X), Content =:= rclref_object:content(X)],
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, N, rclref),
    lists:foreach(fun ({IndexNode, _}) ->
                          case lists:member(IndexNode, OkNodesIndexes) of
                            false ->
                                logger:info("Sending repair RObj: ~p to IndexNode: ~p",
                                            [RObj, IndexNode]),
                                riak_core_vnode_master:command(IndexNode,
                                                               {repair_request, RObj},
                                                               rclref_vnode_master);
                            _ ->
                                ok
                          end
                  end,
                  PrefList),
    ok.

```


### coverage coordinator

The coverage coordinator is implmeneted using `riak_core_coverage_fsm` behaviour.
In rclref, there are two types of coverage calls.

1. unique
This coverage call will request for one replica per key. This means that the coverage plan aims to send the request to a minimum number of vnode which can cover all the unique keys existing in the backend.

2. all
This coverage call will request for all replicas per key. This means that the coverage plan aims to send the request to **ALL** the vnodes which is compuationally very expensive.

You can see the difference between them in the return value of the `init` funciton. Read [this page](https://github.com/lambdaclass/riak_core_tutorial#6-coverage-commands) for more details.

```erlang
-module(rclref_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-export([start_link/1]).
-export([init/2, process_results/2, finish/2]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).

-record(state, {req_id, client_pid, request, accum = []}).

start_link([ReqId, Client_Pid, _Client_Node, Request, Options]) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_COVERAGE),
    riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, Client_Pid}, [Request, Timeout]).

% Callbacks
% Client_Pid: Pid, Client_Node: Node
init({pid, ReqId, Client_Pid}, [Request = {unique, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, client_pid = Client_Pid, request = Request, accum = []},
    {Request, allup, ?N, 1, rclref, rclref_vnode_master, Timeout, State};
init({pid, ReqId, Client_Pid}, [Request = {all, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, client_pid = Client_Pid, request = Request, accum = []},
    {Request, allup, ?N, ?N, rclref, rclref_vnode_master, Timeout, State}.

process_results({{_ReqId, {_Partition, _Node}}, []}, State) ->
    {done, State};
process_results({{_ReqId, {_Partition, _Node}}, Data}, State = #state{accum = Accum}) ->
    % If you need to get which partition and node the data comes from
    % NewAccum = [{Partition, Node, Data} |  Accum],
    NewAccum = Data ++ Accum,
    {done, State#state{accum = NewAccum}}.

finish(clean,
       State =
           #state{req_id = ReqId, request = {unique, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    NewAccum = lists:usort(Accum),
    Client_Pid ! {ReqId, {ok, NewAccum}},
    {stop, normal, State};
finish(clean,
       State =
           #state{req_id = ReqId, request = {all, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    Client_Pid ! {ReqId, {ok, Accum}},
    {stop, normal, State};
finish({error, Reason},
       State =
           #state{req_id = ReqId, request = {unique, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    NewAccum = lists:usort(Accum),
    Client_Pid ! {ReqId, {partial, Reason, NewAccum}},
    {stop, normal, State};
finish({error, Reason},
       State =
           #state{req_id = ReqId, request = {all, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    Client_Pid ! {ReqId, {partial, Reason, Accum}},
    {stop, normal, State}.

```

`process_results` function will be used to concatenate results from different vnodes.
After receiving all the responses,  `finish` will be called. This will send back the coverage result to the client.
