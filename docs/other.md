# Other

This page provides information on stuff that has not been explained in other pages.

## How to configure rclref?

Configuring rclref is done in mostly in `rclref.app.src`. Environemntal values are defined in this module.
In order to check bugs in the config as earily as possible, environmental values are read using the `rclref_config` module.

## How are vector clocks implemented?

Please read the [wiki](https://en.wikipedia.org/wiki/Vector_clock) if you don't know what vector clocks do.

When a client on node1 requests to put a new key-value in the database, a vector clock `[{node1, 1}]` will be created and stored together with the value.
Then if another client on node2 requests to put a value on the same key, the vector clock will be updated to `[{node1, 1}, {node2, 1}]` and stored together with the new value.

These vector clock operations are implemeted in the [`rclref_object.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_object.erl) module using the [vector clock library](https://github.com/AntidoteDB/vectorclock).

```erlang
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
```

It is not always the case, however, that there is an order between two vector clocks. For example, `[{node1, 2}, {node2, 1}]` and `[{node1, 1}, {node2, 2}]` have no orders between them.  When updates happen concurrently, there is no causality between the two. On such occastion, it is necessary to have a strategy to merge two objects or a heuristic to select one from them or a strategy for not dealing with the conflict i.e. simply return two different values so that the client can resolve the conflict. 

In rclref, no heuristics were implemented for selecting the value on concurrent updates. For simplicity, when concurrent updates happen, it just chooses either of the values. This is implemented in the merge function in [`rclref_object.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_object.erl) module.

```erlang
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
```

## How to delete a tombstone from backend?

Tombstone is a key-value which has an undefined value. 
Tombstone is created when a client request a delete on a Key because delete request is internally converetd into a put request with key=Key, value=undefined. Tombstones are not transparent to the user but you can see them using the coverage calls in LowLevelAPI such as `rclref:list_all_objects()`.

The LowLevelAPI also provides a method to delete the tombstone completely from the backend which is `rclref:reap(Key)`. This function should only be used when the connection between nodes is stable otherwise the deleted value might resurrect.


## How to use riak_core instead of riak_core_lite?

1. Remove riak_core_lite and riak_core_lite_utils from rebar.config
2. Add riak_core as dependency.
3. Make a _checkouts directory in rclref. Clone riak_core into that and checkout branch develop-3.0
4. Comment riak_core_coverage_fsm.erl and riak_core_coverage_plan.erl out
5.  Add riak_core.schema to dir _build/dev1/rel/rclref/lib manually
6. Use at most Erlang 22

## More information on riak_core

These are websites that explain how to create riak_core applicaiton.
Note that riak_core and riak_core_lite have some differences e.g. coverage calls.

- [Riak Core wiki](https://github.com/basho/riak_core/wiki)
- [Riak Core Tutorial: lambda class](https://github.com/lambdaclass/riak_core_tutorial)
- [Riak Core Tutorial: Mariano Guerra](https://marianoguerra.github.io/riak-core-tutorial/)
- [A Gentle Introduction to Riak Core: Enrique Fernandez](http://efcasado.github.io/)
- [TryTryTry](https://github.com/rzezeski/try-try-try/tree/master/2011)


