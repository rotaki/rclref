# Backend

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.

As shown in the diagram, each vnode will have their own backend to store the key-value.

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


## Backend Behaviour

In order to make a generic backend that can support storages engines like `ets` or `dets` or even other modules depending on the user's preference, we will first implement a backend behaviour called [`rclref_backend.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_backend.erl). In this module, we will declare functions that are essential for a backend to work.

```erlang
-module(rclref_backend).

-type state() :: term().
-type fold_keys_fun() :: fun((term(), any()) -> any() | no_return()).
-type fold_objects_fun() :: fun((term(), term(), any()) -> any() | no_return()).
-type fold_acc() :: term().
-type fold_opts() :: [term()].
-type fold_result() :: {ok, fold_acc()} | {async, fun()} | {error, term()}.

-callback start(PartitionIndex :: non_neg_integer(), Config :: [{atom(), term()}]) ->
                   {ok, state()}.
-callback stop(state()) -> ok.
-callback get(rclref_object:key(), state()) ->
                 {ok, Value :: term(), state()} |
                 {ok, not_found, state()} |
                 {error, term(), state()}.
-callback put(rclref_object:key(), Value :: binary(), state()) ->
                 {ok, state()} | {error, term(), state()}.
-callback delete(rclref_object:key(), state()) ->
                    {ok, state()} | {error, term(), state()}.
-callback drop(state()) -> {ok, state()} | {error, term(), state()}.
-callback fold_keys(fold_keys_fun(), fold_acc(), fold_opts(), state()) -> fold_result().
-callback fold_objects(fold_objects_fun(), fold_acc(), fold_opts(), state()) ->
                          fold_result().
-callback is_empty(state()) -> boolean() | {error, term()}.
-callback status(state()) -> [{atom(), term()}].
```


## ETS backend

After implementing `rclref_backend.erl`, we need to implement an actual backend that utilizes this behaviour. [`rclref_ets_backend.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_ets_backend.erl) is a sample implementation of a backend using Erlang Term Storage (ets).


```erlang
-module(rclref_ets_backend).

-behaviour(rclref_backend).

-record(state, {table_id}).

-export([start/2, stop/1, get/2, put/3, delete/2, drop/1, fold_keys/3, fold_keys/4,
         fold_objects/3, fold_objects/4, is_empty/1, status/1]).

start(_PartitionIndex, _Config) ->
    TableId = ets:new(?MODULE, [set, {write_concurrency, false}, {read_concurrency, false}]),
    State = #state{table_id = TableId},
    {ok, State}.

stop(_State = #state{table_id = TableId}) ->
    true = ets:delete(TableId),
    ok.

get(Key, State = #state{table_id = TableId}) ->
    case ets:lookup(TableId, Key) of
      [] ->
          {ok, not_found, State};
      [{_, Value}] ->
          {ok, Value, State};
      Reason ->
          {error, Reason, State}
    end.

put(Key, Value, State = #state{table_id = TableId}) ->
    true = ets:insert(TableId, {Key, Value}),
    {ok, State}.

delete(Key, State = #state{table_id = TableId}) ->
    true = ets:delete(TableId, Key),
    {ok, State}.

drop(State = #state{table_id = TableId}) ->
    true = ets:delete_all_objects(TableId),
    {ok, State}.

is_empty(_State = #state{table_id = TableId}) ->
    ets:first(TableId) =:= '$end_of_table'.

fold_keys(Fun, Acc, State) ->
    fold_keys(Fun, Acc, [], State).

fold_keys(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    FoldKeysFun =
        fun ({K, _}, A) ->
                Fun(K, A)
        end,
    ets:foldl(FoldKeysFun, Acc0, TableId).

fold_objects(Fun, Acc, State) ->
    fold_objects(Fun, Acc, [], State).

fold_objects(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    FoldObjectsFun =
        fun ({K, V}, A) ->
                Fun(K, V, A)
        end,
    ets:foldl(FoldObjectsFun, Acc0, TableId).

status(_State = #state{table_id = TableId}) ->
    ets:info(TableId).
```

## DETS backend

Implementation using disk-based term storage (dets) is also provided in [`rclref_dets_backend`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_dets_backend.erl).
 
```erlang
 -module(rclref_dets_backend).

-behaviour(rclref_backend).

-record(state, {table_id}).

-export([start/2, stop/1, get/2, put/3, delete/2, drop/1, fold_keys/3, fold_keys/4,
         fold_objects/3, fold_objects/4, is_empty/1, status/1]).

start(Partition, _Config) ->
    {ok, TableId} = dets:open_file(integer_to_list(Partition), [{type, set}]),
    State = #state{table_id = TableId},
    {ok, State}.

stop(_State = #state{table_id = TableId}) ->
    ok = dets:close(TableId),
    ok.

get(Key, State = #state{table_id = TableId}) ->
    case dets:lookup(TableId, Key) of
      [] ->
          {ok, not_found, State};
      [{_, Value}] ->
          {ok, Value, State};
      Reason ->
          {error, Reason, State}
    end.

put(Key, Value, State = #state{table_id = TableId}) ->
    ok = dets:insert(TableId, {Key, Value}),
    {ok, State}.

delete(Key, State = #state{table_id = TableId}) ->
    ok = dets:delete(TableId, Key),
    {ok, State}.

drop(State = #state{table_id = TableId}) ->
    ok = dets:delete_all_objects(TableId),
    {ok, State}.

is_empty(_State = #state{table_id = TableId}) ->
    dets:first(TableId) =:= '$end_of_table'.

fold_keys(Fun, Acc, State) ->
    fold_keys(Fun, Acc, [], State).

fold_keys(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    FoldKeysFun =
        fun ({K, _}, A) ->
                Fun(K, A)
        end,
    dets:foldl(FoldKeysFun, Acc0, TableId).

fold_objects(Fun, Acc, State) ->
    fold_objects(Fun, Acc, [], State).

fold_objects(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    FoldObjectsFun =
        fun ({K, V}, A) ->
                Fun(K, V, A)
        end,
    dets:foldl(FoldObjectsFun, Acc0, TableId).

status(_State = #state{table_id = TableId}) ->
    dets:info(TableId).
```
