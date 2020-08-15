-module(rclref_client).

-export([put/2, put/3, get/1, get/2, delete/1, delete/2, list_keys/0, list_keys/1]).

-spec put(rclref_object:key(), rclref_object:value()) ->
             ok | {error, timeout} | {error, partial} | {error, [term()]}.
put(Key, Value) ->
    put(Key, Value, []).

-spec put(rclref_object:key(), rclref_object:value(), [term()]) ->
             ok | {error, timeout} | {error, partial} | {error, [term()]}.
put(Key, Value, Options) when is_list(Options) ->
    RObj = rclref_object:new(Key, Value),
    case rclref:put(RObj) of
      {ok, _} ->
          ok;
      {error, timeout} ->
          {error, timeout};
      {error, Items} ->
          case contain_robj(Items) of
            true ->
                {error, partial};
            _ ->
                Reasons = [rclref_object:error_reason(Item) || Item <- Items],
                {error, Reasons}
          end
    end.

-spec get(rclref_object:key()) ->
             {ok, [rclref_object:value()]} |
             {error, timeout} |
             {error, partial} |
             {error, not_found} |
             {error, [term()]}.
get(Key) ->
    get(Key, []).

-spec get(rclref_object:key(), [term()]) ->
             {ok, [rclref_object:value()]} |
             {error, timeout} |
             {error, partial} |
             {error, not_found} |
             {error, [term()]}.
get(Key, Options) when is_list(Options) ->
    case rclref:get(Key, Options) of
      {ok, RObjs} ->
          case lists:all(fun (RObj) ->
                                 undefined =:= rclref_object:value(RObj)
                         end,
                         RObjs)
              of
            true ->
                {error, not_found};
            _ ->
                {ok,
                 [rclref_object:value(RObj)
                  || RObj <- RObjs, undefined =/= rclref_object:value(RObj)]}
          end;
      {error, timeout} ->
          {error, timeout};
      {error, Items} ->
          case contain_robj(Items) of
            true ->
                {error, partial};
            _ ->
                Reasons = [rclref_object:error_reason(Item) || Item <- Items],
                case lists:all(fun (Reason) ->
                                       Reason =:= not_found
                               end,
                               Reasons)
                    of
                  true ->
                      {error, not_found};
                  _ ->
                      {error, Reasons}
                end
          end
    end.

-spec delete(rclref_object:key()) ->
                ok | {error, timeout} | {error, partial} | {error, [term()]}.
delete(Key) ->
    delete(Key, []).

-spec delete(rclref_object:key(), [term()]) ->
                ok | {error, timeout} | {error, partial} | {error, [term()]}.
delete(Key, Options) when is_list(Options) ->
    put(Key, undefined, Options).

-spec list_keys() -> {ok, [rclref_object:key()]}.
list_keys() ->
    list_keys([]).

-spec list_keys([term()]) -> {ok, [rclref_object:key()]}.
list_keys(Options) when is_list(Options) ->
    {ok, RObjs} = rclref:list_unique_objects(Options),
    Keys =
        [rclref_object:key(RObj) || RObj <- RObjs, undefined =/= rclref_object:value(RObj)],
    {ok, lists:usort(Keys)}.

-spec contain_robj([rclref_object:riak_object() | rclref_object:vnode_error()]) ->
                      boolean().
contain_robj([Item | Items]) ->
    case rclref_object:is_robj(Item) of
      true ->
          true;
      false ->
          contain_robj(Items)
    end;
contain_robj([]) ->
    false.
