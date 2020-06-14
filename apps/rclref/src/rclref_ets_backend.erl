-module(rclref_ets_backend).

-behaviour(rclref_backend).

-record(state, {table_id}).

-export([start/2, stop/1, get/2, put/3, delete/2, drop/1, fold_keys/4, fold_objects/4,
         is_empty/1, status/1]).

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
    case ets:insert(TableId, {Key, Value}) of
      true ->
          {ok, State};
      Reason ->
          {error, Reason, State}
    end.

delete(Key, State = #state{table_id = TableId}) ->
    case ets:delete(TableId, Key) of
      true ->
          {ok, State};
      Reason ->
          {error, Reason}
    end.

drop(State = #state{table_id = TableId}) ->
    case ets:delete_all_objects(TableId) of
      true ->
          {ok, State};
      Reason ->
          {error, Reason}
    end.

is_empty(_State = #state{table_id = TableId}) ->
    ets:first(TableId) =:= '$end_of_table'.

fold_keys(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    ets:foldl(Fun, Acc0, TableId).

fold_objects(Fun, Acc0, _Options, _State = #state{table_id = TableId}) ->
    ets:foldl(Fun, Acc0, TableId).

status(_State = #state{table_id = TableId}) ->
    ets:info(TableId).