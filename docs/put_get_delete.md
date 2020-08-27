# How are put, get, delete implemented in rclref?

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.

This page provides an overview of how put, get and delete are implemented in rclref.

## Code flow

Put, get and delete have almost the same code flow. 
The code flow of `rclref_client:get(Key)` is shown in the following diagram.

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

The main component of rclref is shown in the diagram above.
When a user commands `rclref_client:get(Key)`, it will start a supervisor which manages a coodinator in `simle one for one` strategy. Then the coordinator will ask the vnodes for the requested data and send it back to the API module once it has collected a certain number of responses. Let's look at how each part of them are implemented from bottom up.

## Backend

Two types of backend is provided in rclref, which are ETS and DETS.
ETS is short for Erlang Term Storage which is an in-memory storage that can store erlang terms. DETS is short for Disk ETS which is an disk based persistent storage with almost the same interface as ETS. Since DETS store data in the disk, it is much slower than ETS but has smaller memory footprint.

Read [here](backend.md) for implementation details.

## Vnodes

The main feature of riak_core(riak_core_lite) is to distribute client requests to processes in the nodes in the cluster. These processes are often referred to as virtual nodes (vnodes). The number of vnodes in a cluster is dependent on the size of the ring of that cluster. A ring is divided into a fixed number of partitions and each vnode is responsible for one of them. 

When a client makes a request, a hash will be calculated from a client’s request denoting which partition of the ring (thus, vnode) is responsible for handling the request. This is called consistent hashing. With consistent hashing, the following can be achieved.

- Even distribution of key workload between vnodes.
- Smooth adaption to dynamic changes in the cluster by replication of data.

A detailed explanation of consistent hashing is provided [here](http://blog.carlosgaldino.com/consistent-hashing.html).

In rclref, a vnode handles the following requests.

- put, get, delete request
- handoff request
- coverage request

Read [here](vnodes.md) for implementation details.

## Coordinator

Requests made by a client are handled by a coordinator. The coordinator will interact with the vnodes by sending and receiving the requests. For example, if it receives a put request, it generates a hash to determine which vnodes to send the requests to and send it to them. Usually, the coordinator will send the put request to multiple vnodes so that multiple copies of the object exist in the cluster. This is called replication and this ensures the fault tolerance of the database. 

Read [here](coordinator.md) for implementation details.

## Supervisor

Supervisors are used to manage the coordinators. They will restart the coordinator process when needed.

Read [here](supervisor.md) for implementation details.


## API

In rclref, three APIs are provided that can be used to put, get and delete an object from the backend. 

- LowLevelAPI
- UserAPI
- HttpAPI

It is recommended that the user only use the UserLevelAPI and HttpAPI for manipulating the database. LowLevelAPI should only be used for debugging.
The usage of UserAPI and HttpAPI is provided [here](usage.md).

### LowLevelAPI

LowLevelAPI is provided by `rclref.erl` module. This API should only be used in the case of debugging because it reveals detailed information about the object on put, get and delete which should be transparent to the user of rclref. Such as

- node
- partition number
- vector clock 

In addition, some queries are exclusive to this module such as

- reap
- list_all_keys
- list_all_objects

Read [here](lowlevelapi.md) for implementation details.

### UserAPI

UserAPI is provided by `rclref_client.erl` module. Compared with the LowLevelAPI, this API reveals less information on put, get, and delete. 

Read [here](userapi.md) for implementation details.

### HttpAPI

HttpAPI is provided using the `rclref_http_handler.erl` using the Cowboy library. This API reveals the same amount of information on put, get, and delete as the UserAPI.

Read [here](httpapi.md) for implementation details.


