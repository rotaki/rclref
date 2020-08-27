# Backend

As shown in the diagram in TODO, each vnode will have their own backend to store the key-value.

## Backend Behaviour

In order to make a generic backend that can support storages engines like `ets` or `dets` or even other modules depending on user preference, we will first implement a backend behaviour called `rclref_backend.erl`. In this module, we will declare functions that are essential for a backend to work.

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

After implementing `rclref_backend.erl`, we need to implement an actual backend that utilizes this behaviour. A sample implementation using Erlang Term Storage (ets) is given in the following snippet. Implementation using disk-based term storage (dets) is also provided in the repository.


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


Now let's look at the implementation of vnodes to see how they use these backend modules.
