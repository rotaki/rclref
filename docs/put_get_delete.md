# put, get, delete

This page shows provides an overview of how put, get and delete are implemented in rclref.

## How are put, get and delete implemented?

Put, get and delete have almost the same code flow. 
The code flow of `rclref_client:get(Key)` is shown in the following diagram.

```plantuml
title rclref_client:get(Key)

[*] --> UserAPI 
UserAPI -down-> API: get request
API -down-> Supervisor
Supervisor -down-> Coordinator : simple one for one
Coordinator --> Vnode1
Coordinator --> Vnode2
Coordinator --> Vnode3
Vnode1 --> Coordinator
Vnode2 --> Coordinator
Vnode3 --> Coordinator

Coordinator -right-> API
API -up-> UserAPI

note right of Coordinator
    When enough responses from vnodes are collected, return to API
end note

UserAPI: rclref_client.erl
API: rclref.erl
Supervisor: rclref_get_statem_sup.erl
Coordinator: rclref_get_statem.erl
Vnode1: rclref_vnode.erl
Vnode1: rclref_ets_backend.erl
Vnode2: rclref_vnode.erl
Vnode2: rclref_ets_backend.erl
Vnode3: rclref_vnode.erl
Vnode3: rclref_ets_backend.erl
```

The usage of the UserAPI (rclref_client.erl) is described in TODO section.
When a user commands `rclref_client:get(Key)`, it will start a supervisor which manages a get coodinator in `simle one for one` strategy. Then the coordinator will ask the vnodes for data and send it back to the API module once it has collected a certain number of responses. Lets look at how each part of them are implemented in more detail.

## UserAPI

The following is a snippet from [rclref_client.erl](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_client.erl).

```erl
-spec get(rclref_object:key(), [term()]) ->
             {ok, [rclref_object:value()]} |
             {error, timeout} |
             {error, partial} |
             {error, not_found} |
             {error, [term()]}.
get(Key, Options) when is_list(Options) ->
    case rclref:get(Key, Options) of
      {ok, RObjs} ->
          {ok, [rclref_object:value(RObj) || RObj <- RObjs]};
      {error, timeout} ->
          {error, timeout};
      {{ok, []}, {error, VnodeErrors}} ->
          % If all the errors are not_found, return not_found. Otherwise return all errors.
          Reasons = [rclref_object:error_reason(VnodeError) || VnodeError <- VnodeErrors],
          case lists:all(fun (Reason) ->
                                 Reason =:= not_found
                         end,
                         Reasons)
              of
            true ->
                {error, not_found};
            _ ->
                {error, Reasons}
          end;
      {{ok, _RObjs}, {error, _VnodeErrors}} ->
          {error, partial}
    end.
```



## API

## Supervisor

## Coordinator
A coordnator is a process that commnunicate with the vnodes. It is implemented using `gen_statem`.
When a request is issued from the user, it will generate a hash to determin which vnodes to send the request to and send it to them. Usually, the coordinator will send the request to multiple vnodes. 



## Vnodes
The main feature of riak_core is to distribute client requests to processes in the nodes in the cluster. These processes are often referred to as virtual nodes (vnodes). The number of vnodes in a cluster is dependent on the size of the ring of that cluster.  A ring is divided into a fixed number of partitions and each vnode is responsible for one of them. A hash will be calculated from a client’s request denoting which partition of the ring (thus, vnode) is responsible for handling the request. This is called consistent hashing. With consistent hashing, the following can be achieved.

- Even distribution of key workload between vnodes.
- Smooth adaption to dynamic changes in the cluster by replication of data.

A detailed explanation of consistent hashing is provided [here](http://blog.carlosgaldino.com/consistent-hashing.html).

In rclref, a vnode does a lot of things.

- handle put, get, delete request
- handle handoff request
- handle coverage request

This page will explain how put, get and delete are implemented in rclref.



