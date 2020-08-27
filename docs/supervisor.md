# Supervisor

This page provides an overview of how supervisors are implemented in rclref.

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.


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

## What does a supervisor do?

Supervisor in rclref supervises the coordinators.
Since there are 3 coordinators, there are 3 supervisors as well.

| Coordinator                  | Supervisor                    |
| ---------------------------  | ----------------------------- |
| `rclref_put_statem.erl`      | `rclref_put_statem_sup.erl`   |
| `rclref_get_statem.erl`      | `rclref_get_statem_sup.erl`   |
| `riak_core_coverage_fsm.erl` | `rclref_coverage_fsm_sup.erl` |

There is also another supervisor that supervises these supervisors. These supevisors are activated on starting up the application by this supervisor. This is defined in [`rclref_sup.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_sup.erl).

```erlang
-module(rclref_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init(_Args) ->
    VMaster =
        {rclref_vnode_master,
         {riak_core_vnode_master, start_link, [rclref_vnode]},
         permanent,
         5000,
         worker,
         [riak_core_vnode_master]},
    PutStatem =
        {rclref_put_statem_sup,
         {rclref_put_statem_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_put_statem_sup]},
    GetStatem =
        {rclref_get_statem_sup,
         {rclref_get_statem_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_get_statem_sup]},
    CoverageFsm =
        {rclref_coverage_fsm_sup,
         {rclref_coverage_fsm_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_coverage_fsm_sup]},
    {ok, {{one_for_one, 5, 10}, [VMaster, PutStatem, GetStatem, CoverageFsm]}}.
```

## put supervisor

A put supervisor is defined in [`rclref_put_statem_sup.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_put_statem_sup.erl). 

```erlang
-module(rclref_put_statem_sup).

-behaviour(supervisor).

-export([start_put_statem/1, stop_put_statem/1, start_link/0]).
-export([init/1]).

start_put_statem(Args) ->
    ReqId = reqid(),
    {ok, _} = supervisor:start_child(?MODULE, [[ReqId] ++ Args]),
    {ok, ReqId}.

stop_put_statem(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok = supervisor:delete_child(?MODULE, Pid).

start_link() ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init([]) ->
    PutStatem =
        {undefined,
         {rclref_put_statem, start_link, []},
         temporary,
         5000,
         worker,
         [rclref_put_statem]},

    {ok, {{simple_one_for_one, 10, 10}, [PutStatem]}}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
```

The important function in the snippet above is the `start_put_statem/1`. This function will be called by the LowLevelAPI to start up the coordinator 

