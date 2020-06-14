-module(rclref_get_statem).

-behaviour(gen_statem).

-export([get/2]).
-export([start_link/1, stop/2]).
-export([done_get/2, fail_get/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

-define(N, 3).
-define(W, 3).
-define(R, 3).
% timeout per state 10 seconds
-define(TIMEOUT, 10000).

-record(state, {req_id, from, client, key, values = [], preflist, num_r = 0, num_w = 0}).

% Call the supervisor to start the statem
get(Client, Key) ->
    ReqId = reqid(),
    {ok, _} = rclref_get_statem_sup:start_get_statem([ReqId, self(), Client, Key]),
    {ok, ReqId}.

% Start and Stop
start_link([ReqId, From, Client, Key]) ->
    logger:info("(start_link) starting get statem"),
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key], []).

stop(Pid, Reason) ->
    logger:info("(stop) stopping get statem"),
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
done_get(Pid, Value) ->
    logger:info("(done_get) Pid: ~p", [Pid]),
    gen_statem:cast(Pid, {done_get, Value}).

fail_get(Pid) ->
    logger:info("(fail_get) Pid: ~p", [Pid]),
    gen_statem:cast(Pid, fail_get).

% Callbacks
init([ReqId, From, Client, Key]) ->
    logger:info("(init) initializing get statem"),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId,
                   from = From,
                   client = Client,
                   key = Key,
                   preflist = PrefList},
    Fn = fun(IndexNode) -> 
                 riak_core_vnode_master:command(IndexNode,
                                                {kv_get_request, Key, self()},
                                                rclref_vnode_master)
         end,
    [Fn(IndexNode) || {IndexNode, _Type} <- PrefList],
    {ok, waiting, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

callback_mode() ->
    logger:info("(callback_mode)"),
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    logger:info("(code_change)"),
    {ok, StateName, State}.

terminate(Reason, _StateName, #state{req_id = ReqId, from = From, values = Values}) ->
    logger:info("(terminate) terminating get statem"),
    case Reason of
      normal ->
          From ! {ok, {ReqId, Values}};
      _ ->
          From ! {error, {ReqId, undefined}}
    end,
    rclref_get_statem_sup:stop_get_statem(self()),
    ok.

% State function
waiting(cast, {done_get, Value}, State = #state{num_r = Num_r0, values = Values0}) ->
    logger:info("(waiting) waiting state : done get"),
    Num_r = Num_r0 + 1,
    Values = Values0 ++ [Value],
    NewState = State#state{num_r = Num_r, values = Values},
    case Num_r =:= ?R of
      true ->
          %TODO: Next state for read repair
          {stop, normal, NewState};
      false ->
          {keep_state, NewState, [{state_timeout, ?TIMEOUT, hard_stop}]}
    end;
waiting(cast, fail_get, State = #state{num_r = _Num_r0}) ->
    logger:info("(waiting) waiting state : fail get"),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]};
waiting(state_timeout, hard_stop, State) ->
    logger:info("(waiting) waiting state: timeout"),
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    logger:info("(waiting) waiting state: miscellaneous"),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hardstop}]}.

% Internal Functions
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
