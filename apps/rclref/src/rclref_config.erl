-module(rclref_config).

-include_lib("stdlib/include/assert.hrl").

-export([storage_backend/0, merge_strategy/0, n_val/0, r_val/0, w_val/0, timeout_put/0,
         timeout_get/0, timeout_coverage/0]).

-spec storage_backend() -> ets | other.
storage_backend() ->
    Backends = [ets, other],
    {ok, Backend} = application:get_env(rclref, storage_backend),
    case lists:member(Backend, Backends) of
      true ->
          Backend;
      _ ->
          ?assert(false)
    end.

-spec merge_strategy() -> none | other.
merge_strategy() ->
    MergeStrategies = [none, other],
    {ok, MergeStrategy} = application:get_env(rclref, merge_strategy),
    case lists:member(MergeStrategy, MergeStrategies) of
      true ->
          MergeStrategy;
      _ ->
          ?assert(false)
    end.

-spec n_val() -> non_neg_integer().
n_val() ->
    {ok, N_val} = application:get_env(rclref, n_val),
    case is_integer(N_val) andalso N_val > 0 of
      true ->
          N_val;
      _ ->
          ?assert(false)
    end.

-spec r_val() -> non_neg_integer().
r_val() ->
    {ok, R_val} = application:get_env(rclref, r_val),
    {ok, N_val} = application:get_env(rclref, n_val),
    case is_integer(R_val) andalso N_val >= R_val andalso R_val >= 0 of
      true ->
          R_val;
      _ ->
          ?assert(false)
    end.

-spec w_val() -> non_neg_integer().
w_val() ->
    {ok, W_val} = application:get_env(rclref, w_val),
    {ok, N_val} = application:get_env(rclref, n_val),
    case is_integer(W_val) andalso N_val >= W_val andalso W_val >= 0 of
      true ->
          W_val;
      _ ->
          ?assert(false)
    end.

-spec timeout_put() -> non_neg_integer() | infinity.
timeout_put() ->
    {ok, TimeoutPut} = application:get_env(rclref, timeout_put),
    case is_integer(TimeoutPut) andalso TimeoutPut > 0 orelse TimeoutPut =:= infinity of
      true ->
          TimeoutPut;
      _ ->
          ?assert(false)
    end.

-spec timeout_get() -> non_neg_integer() | infinity.
timeout_get() ->
    {ok, TimeoutGet} = application:get_env(rclref, timeout_get),
    case is_integer(TimeoutGet) andalso TimeoutGet > 0 orelse TimeoutGet =:= infinity of
      true ->
          TimeoutGet;
      _ ->
          ?assert(false)
    end.

-spec timeout_coverage() -> non_neg_integer() | infinity.
timeout_coverage() ->
    {ok, TimeoutCoverage} = application:get_env(rclref, timeout_coverage),
    case is_integer(TimeoutCoverage) andalso TimeoutCoverage > 0 orelse TimeoutCoverage =:= infinity of
        true ->
            TimeoutCoverage;
        _ ->
            ?assert(false)
    end.
