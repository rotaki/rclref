-module(rclref_object).

-export_type([key/0, value/0, riak_object/0, vnode_error/0]).

-type key() :: term().
-type value() :: term().
-type vclock() :: vectorclock:vectorclock().

-record(r_content, {value :: value(), vclock = vectorclock:new() :: vclock()}).
-record(r_object,
        {key :: key(),
         r_content :: #r_content{},
         partition :: non_neg_integer() | undefined,
         node :: node() | undefined}).
-record(r_error,
        {reason :: term(),
         partition :: non_neg_integer() | undefined,
         node :: node() | undefined}).

-opaque riak_object() :: #r_object{}.
-opaque vnode_error() :: #r_error{}.

-export([key/1, content/1, partition/1, node/1, value/1, vclock/1, error_reason/1,
         increment_vclock/2, new_vclock/0, merge/1, new/2, new/4, new_content/2, new_error/3,
         is_robj/1, is_error/1]).

-spec key(riak_object()) -> key().
key(#r_object{key = Key}) ->
    Key.

-spec content(riak_object()) -> #r_content{}.
content(#r_object{r_content = Content}) ->
    Content.

-spec partition(riak_object()) -> non_neg_integer().
partition(#r_object{partition = Partition}) ->
    Partition.

-spec node(riak_object()) -> node().
node(#r_object{node = Node}) ->
    Node.

-spec value(riak_object() | #r_content{}) -> value().
value(#r_object{r_content = #r_content{value = Value}}) ->
    Value;
value(#r_content{value = Value}) ->
    Value.

-spec vclock(riak_object() | #r_content{}) -> vclock().
vclock(#r_object{r_content = #r_content{vclock = VClock}}) ->
    VClock;
vclock(#r_content{vclock = VClock}) ->
    VClock.

-spec error_reason(vnode_error()) -> term().
error_reason(#r_error{reason = Reason}) ->
    Reason.

-spec new_vclock() -> vclock().
new_vclock() ->
    vectorclock:new().

-spec increment_vclock(node(), vclock()) -> vclock().
increment_vclock(Node, VClock) ->
    vectorclock:update_with(Node,
                            fun (X) ->
                                    X + 1
                            end,
                            1,
                            VClock).

-spec new_content(value(), vclock()) -> #r_content{}.
new_content(Value, VClock) ->
    #r_content{value = Value, vclock = VClock}.

-spec new_error(term(), non_neg_integer(), node()) -> vnode_error().
new_error(Reason, Partition, Node) ->
    #r_error{reason = Reason, partition = Partition, node = Node}.

-spec new(key(), value()) -> riak_object().
new(Key, Value) ->
    #r_object{key = Key, r_content = #r_content{value = Value}}.

-spec new(key(), #r_content{}, non_neg_integer(), node()) -> riak_object().
new(Key, Content, Partition, Node) ->
    #r_object{key = Key, r_content = Content, partition = Partition, node = Node}.

-spec merge([riak_object()]) -> riak_object().
merge([RObj]) ->
    RObj;
merge([RObj0, RObj1 | RObjs]) ->
    VClock0 = vclock(RObj0),
    VClock1 = vclock(RObj1),
    NewRObj =
        case vectorclock:le(VClock0, VClock1) of
          true ->
              RObj1;
          false ->
              RObj0
        end,
    merge([NewRObj] ++ RObjs).

is_robj(#r_object{}) ->
    true;
is_robj(_) ->
    false.

is_error(#r_error{}) ->
    true;
is_error(_) ->
    false.
