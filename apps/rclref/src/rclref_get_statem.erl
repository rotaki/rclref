-module(rclref_get_statem).

-behaviour(gen_statem).

-export([get/2]).
-export([start_link/1, stop/2]).
-export([done_get/2, fail_get/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

%TODO: get these numbers from config
-define(N, 3).
-define(W, 3).
-define(R, 3).
% timeout per state 5 seconds
-define(TIMEOUT, 5000).

-record(state, {req_id, from, client, key, values = [], preflist, num_r = 0, num_w = 0}).

% Call the supervisor to start the statem
get(Client, Key) ->
    ReqId = reqid(),
    {ok, _} = rclref_get_statem_sup:start_get_statem([ReqId, self(), Client, Key]),
    {ok, ReqId}.

% Start and Stop
start_link([ReqId, From, Client, Key]) ->
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key], []).

stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
done_get(Pid, Value) ->
    gen_statem:cast(Pid, {done_get, Value}).

fail_get(Pid) ->
    gen_statem:cast(Pid, fail_get).

% Callbacks
init([ReqId, From, Client, Key]) ->
    logger:info("Initializing GetStatem, Pid:~p", [self()]),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId,
                   from = From,
                   client = Client,
                   key = Key,
                   preflist = PrefList},
    Fn = fun (IndexNode) ->
                 riak_core_vnode_master:command(IndexNode,
                                                {kv_get_request, Key, self()},
                                                rclref_vnode_master)
         end,
    [Fn(IndexNode) || {IndexNode, _Type} <- PrefList],
    {ok, waiting, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, #state{req_id = ReqId, from = From, values = Values}) ->
    logger:info("Terminating GetStatem, Pid:~p", [self()]),
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
    logger:debug("GetStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, done_get, Num_r0]),
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
waiting(cast, fail_get, State = #state{num_r = Num_r0}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, fail_get, Num_r0]),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]};
waiting(state_timeout, hard_stop, State) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p", [state_timeout, hard_stop]),
    {stop, waiting_timed_out, State};
waiting(EventType, EventContent, State = #state{}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p", [EventType, EventContent]),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

% Internal Functions
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
