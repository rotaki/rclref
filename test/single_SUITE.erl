-module(single_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([put_get_delete_test/1, coverage_call_test/1]).

-define(N, rclref_config:n_val()).

all() ->
    [put_get_delete_test, coverage_call_test].
    

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    true = riak_utils:is_ring_ready(node()),
    Config.

end_per_suite(Config) ->
    application:stop(rclref),
    Config.

put_get_delete_test(_Config) ->
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

coverage_call_test(_Config) ->
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    RObjs = [rclref_object:new(Key, Value) || {Key, Value} <- lists:zip(Keys, Values)],
    % put 20 key values
    lists:foreach(fun (RObj) ->
                          ok = rclref:put(RObj)
                  end,
                  RObjs),
    % check listing of unique keys
    {ok, lists:usort(Keys)} =:=  rclref:list_unique_keys(),

    % check listing of unique RObjs
    {ok, lists:usort(RObjs)} =:= rclref:list_unique_objects(),

    % check listing of all keys
    {ok, GotKeys} = rclref:list_all_keys(),
    lists:foreach(fun(Key) ->
			?N =:= count(Key, GotKeys)
		end,
		Keys),
    % check listing of all RObjs
    {ok, GotRObjs} = rclref:list_all_objects(),
    lists:foreach(fun(RObj) ->
			?N =:= count(RObj, GotRObjs)
		end,
		RObjs),
    ok.

% count number of occurrences of X in Y
count(X, Y) ->
	length([E || E <- Y, E =:= X]).
	
