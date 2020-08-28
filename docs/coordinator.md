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

