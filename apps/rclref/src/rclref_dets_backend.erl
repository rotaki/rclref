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
