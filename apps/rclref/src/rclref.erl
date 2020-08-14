-module(rclref).

-compile({no_auto_import, [{put, 2}]}).

-export([ping/0, put/1, put/2, get/1, get/2, delete/1, delete/2, reap_tombs/1,
         reap_tombs/2]).
-export([list_unique_keys/0, list_unique_keys/1]).
-export([list_all_keys/0, list_all_keys/1, list_all_objects/0, list_all_objects/1]).

-ignore_xref([{ping, 0}]).

-define(TIMEOUT_PUT, rclref_config:timeout_put()).
-define(TIMEOUT_GET, rclref_config:timeout_get()).
-define(TIMEOUT_REAP_TOMBS, 100000).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).
-define(N, rclref_config:n_val()).

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

-spec put(rclref_object:riak_object()) ->
             {ok, [rclref_object:riak_object()]} |
             {error, [rclref_object:riak_object() | rclref_object:vnode_error()]} |
             {error, timeout}.
put(RObj) ->
    put(RObj, []).

-spec put(rclref_object:riak_object(), Options :: [term()]) ->
             {ok, [rclref_object:riak_object()]} |
             {error, [rclref_object:riak_object() | rclref_object:vnode_error()]} |
             {error, timeout}.
put(RObj, Options) when is_list(Options) ->
    {ok, ReqId} = rclref_put_statem_sup:start_put_statem([self(), node(), RObj, Options]),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_PUT),
    wait_for_reqid(ReqId, Timeout).

-spec get(rclref_object:key()) ->
             {ok, [rclref_object:riak_object()]} |
             {error, [rclref_object:riak_object() | rclref_object:errro()]} |
             {error, not_found} |
             {error, timeout}.
get(Key) ->
    get(Key, []).

-spec get(rclref_object:key(), Options :: [term()]) ->
             {ok, [rclref_object:riak_object()]} |
             {error, [rclref_object:riak_object() | rclref_object:errro()]} |
             {error, not_found} |
             {error, timeout}.
get(Key, Options) when is_list(Options) ->
    {ok, ReqId} = rclref_get_statem_sup:start_get_statem([self(), node(), Key, Options]),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_GET),
    wait_for_reqid(ReqId, Timeout).

-spec delete(rclref_object:key()) ->
                {ok, [rclref_object:riak_object()]} |
                {error, [rclref_object:riak_object() | rclref_object:vnode_error()]} |
                {error, timeout}.
delete(Key) ->
    % keep it as a tombstone
    delete(Key, []).

-spec delete(riak_obejct:key(), Options :: [term()]) ->
                {ok, [rclref_object:riak_object()]} |
                {error, [rclref_object:riak_object() | rclref_object:vnode_error()]} |
                {error, timeout}.
delete(Key, Options) when is_list(Options) ->
    RObj = rclref_object:new(Key, undefined),
    put(RObj, Options).

-spec reap_tombs(rclref_object:key()) -> ok.
reap_tombs(Key) ->
    reap_tombs(Key, []).

-spec reap_tombs(rclref_object:key(), Options :: [term()]) -> ok.
reap_tombs(Key, Options) when is_list(Options) ->
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    lists:foreach(fun ({IndexNode, _}) ->
                          riak_core_vnode_master:command(IndexNode,
                                                         {reap_tombs_request, Key},
                                                         rclref_vnode_master)
                  end,
                  PrefList),
    ok.

-spec list_unique_keys() -> {ok, [rclref_object:key()]}.
list_unique_keys() ->
    list_unique_keys([]).

-spec list_unique_keys(Options :: [term()]) -> {ok, [rclref_object:key()]}.
list_unique_keys(Options) when is_list(Options) ->
    coverage_request({unique, keys}, Options).

-spec list_all_keys() -> {ok, [rclref_object:key()]}.
list_all_keys() ->
    list_all_keys([]).

-spec list_all_keys(Options :: [term()]) -> {ok, [rclref_object:key()]}.
list_all_keys(Options) when is_list(Options) ->
    coverage_request({all, keys}, Options).

-spec list_all_objects() -> {ok, [rclref_object:object()]}.
list_all_objects() ->
    list_all_objects([]).

-spec list_all_objects(Options :: [term()]) -> {ok, [rclref_object:object()]}.
list_all_objects(Options) when is_list(Options) ->
    coverage_request({all, objects}, Options).

% private
-spec coverage_request(term(), [term()]) ->
                          {error, timeout} |
                          {ok, [rclref_object:key()]} |
                          {ok, [rclref_object:object()]}.
coverage_request(Request, Options) ->
    {ok, ReqId} =
        rclref_coverage_fsm_sup:start_coverage_fsm([self(), node(), Request, Options]),
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
