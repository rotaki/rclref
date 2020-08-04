-module(rclref).

-compile({no_auto_import, [{put, 2}]}).

-export([ping/0, put/1, put/2, get/1, get/2, delete/1]).
-export([list_keys/0, list_keys/1, list_all/0, list_all/1]).


-ignore_xref([{ping, 0}]).

-define(TIMEOUT_PUT, rclref_config:timeout_put()).
-define(TIMEOUT_GET, rclref_config:timeout_get()).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).

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

-spec put(rclref_object:riak_object()) -> ok | {error, timeout} | {error, term()}.
put(RObj) ->
    put(RObj, []).

-spec put(rclref_object:riak_object(), Options :: [term()]) ->
             ok | {error, timeout} | {error, term()}.
put(RObj, Options) when is_list(Options) ->
    {ok, ReqId} = rclref_put_statem:put(node(), RObj, Options),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_PUT),
    wait_for_reqid(ReqId, Timeout).

-spec get(rclref_object:key()) ->
             {ok, rclref_object:riak_object()} |
             {error, not_found} |
             {error, timeout} |
             {error, term()}.
get(Key) ->
    get(Key, []).

-spec get(rclref_object:key(), Options :: [term()]) ->
             {ok, [rclref_object:riak_object()]} |
             {error, not_found} |
             {error, timeout} |
             {error, term()}.
get(Key, Options) when is_list(Options) ->
    {ok, ReqId} = rclref_get_statem:get(node(), Key, Options),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_GET),
    wait_for_reqid(ReqId, Timeout).

-spec delete(rclref_object:key()) -> ok | {error, timeout} | {error, term()}.
delete(Key) ->
    % keep it as a tombstone
    delete(Key, []).

-spec delete(riak_obejct:key(), Options :: [term()]) ->
                ok | {error, timeout} | {error, term()}.
delete(Key, Options) ->
    RObj = rclref_object:new(Key, undefined),
    put(RObj, Options).

list_keys() ->
    list_keys([]).

list_keys(Options) ->
    coverage_command(keys, Options).

list_all() ->
    list_all([]).

list_all(Options) ->
    coverage_command(keyvalues, Options).

% private
coverage_command(Command, Options) ->
    {ok, ReqId} = rclref_coverage_fsm:coverage(node(), Command, Options),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_COVERAGE),
    wait_for_reqid(ReqId, Timeout).

% private
-spec wait_for_reqid(non_neg_integer(), timeout()) -> {error, timeout} | any().
wait_for_reqid(ReqId, Timeout) ->
    receive
      {ReqId, Response} ->
          Response
      after Timeout ->
                {error, timeout}
    end.
