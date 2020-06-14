-module(rclref_backend).

-type state() :: term().
-type fold_keys_fun() :: fun((term(), any()) -> any() | no_return()).
-type fold_objects_fun() :: fun((term(), term(), any()) -> any() | no_return()).
-type fold_acc() :: term().
-type fold_opts() :: [term()].
-type fold_result() :: {ok, fold_acc()} | {async, fun()} | {error, term()}.

-callback start(PartitionIndex :: non_neg_integer(), Config :: [{atom(), term()}]) -> {ok,
                                                                                       state()} |
                                                                                      {error,
                                                                                       term()}.
-callback stop(state()) -> ok.
-callback get(riak_object:key(), state()) -> {ok, Value :: term(), state()} |
                                             {ok, not_found, state()} |
                                             {error, term(), state()}.
-callback put(riak_object:key(), Value :: binary(), state()) -> {ok, state()} |
                                                                {error, term(), state()}.
-callback delete(riak_object:key(), state()) -> {ok, state()} | {error, term(), state()}.
-callback drop(state()) -> {ok, state()} | {error, term(), state()}.
-callback fold_keys(fold_keys_fun(), fold_acc(), fold_opts(), state()) -> fold_result().
-callback fold_objects(fold_objects_fun(),
                       fold_acc(),
                       fold_opts(),
                       state()) -> fold_result().
-callback is_empty(state()) -> boolean() | {error, term()}.
-callback status(state()) -> [{atom(), term()}].
