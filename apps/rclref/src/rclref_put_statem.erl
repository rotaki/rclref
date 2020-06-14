-module(rclref_put_statem).

-behaviour(gen_statem).

-export([put/3]).
-export([start_link/1, stop/2]).
-export([done_put/1, fail_put/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

-define(N, 3).
-define(W, 3).
-define(R, 3).
% timeout per state 10 seconds
-define(TIMEOUT, 10000).

-record(state, {req_id, from, client, key, value, preflist, num_r = 0, num_w = 0}).

%% Call the supervisor to start the statem
put(Client, Key, Value) ->
    ReqId = reqid(),
    {ok, _} = rclref_put_statem_sup:start_put_statem([ReqId, self(), Client, Key, Value]),
    {ok, ReqId}.

%% Start and Stop
start_link([ReqId, From, Client, Key, Value]) ->
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key, Value], []).

stop(Pid, Reason) ->
    logger:info("Stopping PutStatem, Pid:~p", [Pid]),
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
done_put(Pid) ->
    gen_statem:cast(Pid, done_put).

fail_put(Pid) ->
    gen_statem:cast(Pid, fail_put).

% Callbacks
init([ReqId, From, Client, Key, Value]) ->
    logger:info("Initializing PutStatem, Pid:~p", [self()]),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId,
                   from = From,
                   client = Client,
                   key = Key,
                   value = Value,
                   preflist = PrefList},
    Fn = fun (IndexNode) ->
                 riak_core_vnode_master:command(IndexNode,
                                                {kv_put_request, Key, Value, self()},
                                                rclref_vnode_master)
         end,
    [Fn(IndexNode) || {IndexNode, _Type} <- PrefList],
    {ok, waiting, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, #state{req_id = ReqId, from = From}) ->
    case Reason of
      normal ->
          From ! {ok, ReqId};
      _ ->
          From ! {error, ReqId}
    end,
    rclref_put_statem_sup:stop_put_statem(self()),
    ok.

% State function
waiting(cast, done_put, State = #state{num_w = Num_w0}) ->
    logger:info("PutStatem at WAITING state with event ~p:~p, at num_w: ~p",
                [cast, done_put, Num_w0]),
    Num_w = Num_w0 + 1,
    NewState = State#state{num_w = Num_w},
    case Num_w =:= ?W of
      true ->
          {stop, normal, NewState};
      false ->
          {keep_state, NewState, [{state_timeout, ?TIMEOUT, hard_stop}]}
    end;
waiting(cast, fail_put, State = #state{num_w = Num_w0}) ->
    %TODO: count number of failures
    logger:info("PutStatem at WAITING state with event ~p:~p, at num_w: ~p",
                [cast, fail_put, Num_w0]),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]};
waiting(state_timeout, hard_stop, State) ->
    logger:info("PutStatem at WAITING state with event ~p:~p", [state_timeout, hard_stop]),
    {stop, waiting_timed_out, State};
waiting(EventType, EventContent, State = #state{}) ->
    logger:info("PutStatem at WAITING state with event ~p:~p", [EventType, EventContent]),
    {keep_state, State, [{state_timeout, ?TIMEOUT, hard_stop}]}.

% Internal Functions
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
