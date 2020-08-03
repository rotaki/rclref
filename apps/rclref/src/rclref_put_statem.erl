-module(rclref_put_statem).

-behaviour(gen_statem).

-export([put/3]).
-export([start_link/1, stop/2]).
-export([result_of_put/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_PUT, rclref_config:timeout_put()).

-record(state,
        {req_id :: non_neg_integer(),
         from :: pid(),
         client :: node(),
         preflist :: [term()],
         num_ok = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         riak_objects = [] :: [rclref_object:riak_object()]}).

%% Call the supervisor to start the statem
-spec put(Client :: node(), RObj :: rclref_object:riak_object(), Options :: [term()]) ->
             {ok, ReqId :: non_neg_integer()}.
put(Client, RObj, Options) ->
    ReqId = reqid(),
    {ok, _} = rclref_put_statem_sup:start_put_statem([ReqId, self(), Client, RObj, Options]),
    {ok, ReqId}.

%% Start and Stop
-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, From, Client, RObj, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, From, Client, RObj, Options], []).

-spec stop(pid(), term()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec result_of_put(pid(), {ok, ok} | {error, vnode_error}) -> ok.
result_of_put(Pid, Result) ->
    gen_statem:cast(Pid, Result).

% Callbacks
init([ReqId, From, Client, RObj, Options]) ->
    logger:info("Initializing PutStatem, Pid:~p", [self()]),
    Key = rclref_object:key(RObj),
    Value = rclref_object:value(RObj),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_PUT),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId, from = From, client = Client, preflist = PrefList},
    Fn =
        fun (IndexNode) ->
                riak_core_vnode_master:command(IndexNode,
                                               {kv_put_request, Key, Value, self()},
                                               rclref_vnode_master)
        end,
    [Fn(IndexNode) || {IndexNode, _Type} <- PrefList],
    {ok, waiting, State, [{state_timeout, Timeout, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    logger:info("Terminating PutStatem, Pid:~p, Reason:~p", [self(), Reason]),
    ok.

% State function
waiting(cast, {ok, ok}, State = #state{req_id = ReqId, from = From, num_ok = Num_ok0}) ->
    Num_ok = Num_ok0 + 1,
    NewState = State#state{num_ok = Num_ok},
    case Num_ok >= ?W of
      true ->
          From ! {ReqId, ok},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(cast,
        {error, vnode_error},
        State = #state{req_id = ReqId, from = From, num_vnode_error = Num_vnode_error0}) ->
    Num_vnode_error = Num_vnode_error0 + 1,
    NewState = State#state{num_vnode_error = Num_vnode_error},
    case Num_vnode_error > ?N - ?W of
      true ->
          From ! {ReqId, {error, vnode_error}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(state_timeout, hard_stop, State = #state{req_id = ReqId, from = From}) ->
    From ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
