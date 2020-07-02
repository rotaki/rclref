-module(rclref_object).

-export_type([key/0, value/0, riak_object/0]).

-type key() :: term().
-type value() :: term().

-record(r_object, {key :: key(), value :: value()}).

-opaque riak_object() :: #r_object{}.

-export([key/1, value/1, new/2]).

-spec key(riak_object()) -> key().
key(#r_object{key = Key}) ->
    Key.

-spec value(riak_object()) -> value().
value(#r_object{value = Value}) ->
    Value.

-spec new(key(), value()) -> riak_object().
new(Key, Value) ->
    #r_object{key = Key, value = Value}.
