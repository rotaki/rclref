-module(rclref).

-export([ping/0, put/2, get/1]).

-ignore_xref([{ping, 0}]).

%TODO: Get these numbers from config
%TODO: Make state timeout compatible with the base timeout
-define(TIMEOUT_PUT, 2000).
-define(TIMEOUT_GET, 2000).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    % argument to chash_key has to be a two item tuple, since it comes from riak
    % and the full key has a bucket, we use a contant in the bucket position
    % and a timestamp as key so we hit different vnodes on each call
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    % ask for 1 vnode index to send this request to, change N to get more
    % vnodes, for example for replication
    N = 1,
    PrefList = riak_core_apl:get_primary_apl(DocIdx, N, rclref),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rclref_vnode_master).

put(Key, Value) ->
    {ok, ReqId} = rclref_put_statem:put(node(), Key, Value),
    receive
      {ok, ReqId} ->
          logger:info("(put) success");
      {error, ReqId} ->
          logger:info("(put) error");
      {_, _} ->
          logger:error("(put) invalid response")
      after ?TIMEOUT_PUT ->
                logger:error("(put) timeout")
    end.

get(Key) ->
    {ok, ReqId} = rclref_get_statem:get(node(), Key),
    receive
      {ok, {ReqId, Value}} ->
          logger:info("(get) success"),
          Value;
      {error, {ReqId, _}} ->
          logger:info("(get) error");
      {_, _} ->
          logger:error("(get) invalid response")
      after ?TIMEOUT_GET ->
                logger:error("(get) timeout")
    end.
