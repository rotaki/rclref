-module(rclref_object).

-export_type([key/0, value/0, riak_object/0]).

-type key() :: term().
-type value() :: term().

-record(r_object, {key :: key(), value :: value(), partition :: non_neg_integer() | undefined, node :: node() | undefined}).

-opaque riak_object() :: #r_object{}.

-export([key/1, value/1, new/2, new/4]).

-spec key(riak_object()) -> key().
key(#r_object{key = Key}) ->
    Key.

-spec value(riak_object()) -> value().
value(#r_object{value = Value}) ->
    Value.

-spec new(key(), value()) -> riak_object().
new(Key, Value) ->
    #r_object{key = Key, value = Value}.

-spec new(key(), value(), non_neg_integer(), node()) -> riak_object().
new(Key, Value, Partition, Node) ->
    #r_object{key = Key, value = Value, partition = Partition, node = Node}.
