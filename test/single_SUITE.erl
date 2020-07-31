-module(single_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete/1]).

all() ->
    [put_get_delete].
    

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    true = riak_utils:is_ring_ready(node()),
    Config.

end_per_suite(Config) ->
    application:stop(rclref),
    Config.

put_get_delete(_Config) ->
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    lists:foreach(fun (Key) ->
                          {error, not_found} = rclref:get(Key)
                  end,
                  Keys),
    % put 20 key values
    lists:foreach(fun (RObj) ->
                          ok = rclref:put(RObj)
                  end,
                  RObjs),
    % confirm 20 key values
    lists:foreach(fun ({RObj, Key}) ->
                          {ok, GotRObjs} = rclref:get(Key),
                          true = lists:all(fun (GotRObj) ->
                                                   RObj =:= GotRObj
                                           end,
                                           GotRObjs)
                  end,
                  lists:zip(RObjs, Keys)),
    % delete 20 key values
    lists:foreach(fun (Key) ->
                          ok = rclref:delete(Key)
                  end,
                  Keys),
    % confirm deleted
    lists:foreach(fun (Key) ->
                          {error, not_found} = rclref:get(Key)
                  end,
                  Keys),
    ok.
