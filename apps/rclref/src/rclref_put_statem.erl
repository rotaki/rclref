-module(rclref_put_statem).

-behaviour(gen_statem).

-export([put/3]).
-export([start_link/1, stop/2]).
-export([done_put/1, fail_put/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

-define(N, 1).
-define(W, 1).
-define(R, 1).
% timeout per state 10 seconds
-define(TIMEOUT, 20000).

-record(state, {req_id, from, client, key, value, preflist, num_r = 0, num_w = 0}).

%% Call the supervisor to start the statem
put(Client, Key, Value) ->
    ReqId = reqid(),
    {ok, _} = rclref_put_statem_sup:start_put_statem([ReqId, self(), Client, Key, Value]),
    {ok, ReqId}.

%% Start and Stop
start_link([ReqId, From, Client, Key, Value]) ->
    logger:info("(start_link) starting put statem"),
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key, Value], []).

stop(Pid, Reason) ->
    logger:info("(stop) stopping put statem"),
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
done_put(Pid) ->
    logger:info("(done_put) Pid: ~p", [Pid]),
    gen_statem:cast(Pid, {ok, done_put}).

fail_put(Pid) ->
    logger:info("(fail_put) Pid: ~p", [Pid]),
    gen_statem:cast(Pid, {error, fail_put}).

% Callbacks
init([ReqId, From, Client, Key, Value]) ->
    logger:info("(init) initializing put statem"),
    DocIdx = riak_core_util:chash_key({Client, Key}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId,
                   from = From,
                   client = Client,
                   key = Key,
                   value = Value,
                   preflist = PrefList},
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:command(IndexNode,
                                   {kv_put_request, Key, Value, self()},
                                   rclref_vnode_master),
    {ok, waiting, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

callback_mode() ->
    logger:info("(callback_mode)"),
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    logger:info("(code_change)"),
    {ok, StateName, State}.

terminate(Reason, _StateName, #state{req_id = ReqId, from = From}) ->
    logger:info("(terminate) terminating put statem"),
    case Reason of
      normal ->
          From ! {ok, ReqId};
      _ ->
          From ! {error, ReqId}
    end,
    rclref_put_statem_sup:stop_put_statem(self()),
    ok.

% State function
waiting(cast, {ok, done_put}, State = #state{num_w = Num_w0}) ->
    logger:info("(waiting) waiting state : done put"),
    Num_w = Num_w0 + 1,
    NewState = State#state{num_w = Num_w},
    case Num_w =:= ?W of
      true ->
          {stop, normal, NewState};
      false ->
          {keep_state, NewState, [{state_timeout, ?TIMEOUT, hard_stop}]}
    end;
waiting(cast, {error, fail_put}, State = #state{num_w = _Num_w0}) ->
    %TODO: count number of failures
    logger:info("(waiting) waiting state : fail put"),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]};
waiting(state_timeout, hard_stop, State) ->
    logger:info("(waiting) waiting state : time out"),
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    logger:info("(waiting) waiting state : miscellaneous"),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

% Internal Functions
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
