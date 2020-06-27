-module(rclref_put_statem).

-behaviour(gen_statem).

-export([put/3]).
-export([start_link/1, stop/2]).
-export([done_put/1, failed_put/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

-define(N, 3).
-define(W, 3).
-define(R, 3).
% timeout per state 5 seconds
-define(TIMEOUT, 5000).

-record(state,
        {req_id :: non_neg_integer(),
         from :: pid(),
         client :: node(),
         key :: riak_object:key(),
         value :: riak_object:value(),
         preflist :: [term()],
         num_r = 0 :: non_neg_integer(),
         num_w = 0 :: non_neg_integer()}).

%% Call the supervisor to start the statem
-spec put(Client :: node(),
          RObj :: riak_object:riak_object(),
          Options :: [term()]) -> {ok, ReqId :: non_neg_integer()}.
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
-spec done_put(pid()) -> ok.
done_put(Pid) ->
    gen_statem:cast(Pid, done_put).

-spec failed_put(pid()) -> ok.
failed_put(Pid) ->
    gen_statem:cast(Pid, fail_put).

% Callbacks
init([ReqId, From, Client, RObj, Options]) ->
    logger:info("Initializing PutStatem, Pid:~p", [self()]),
    Key = riak_object:key(RObj),
    Value = riak_object:value(RObj),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
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
    {ok, waiting, State, [{state_timeout, Timeout, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    logger:info("Terminating PutStatem, Pid:~p, Reason:~p", [self(), Reason]),
    ok.

% State function
waiting(cast, done_put, State = #state{req_id = ReqId, from = From, num_w = Num_w0}) ->
    logger:debug("PutStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, done_put, Num_w0]),
    Num_w = Num_w0 + 1,
    NewState = State#state{num_w = Num_w},
    case Num_w =:= ?W of
      true ->
          From ! {ReqId, ok},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(cast, failed_put, State = #state{num_w = Num_w0}) ->
    %TODO: count number of failures
    logger:debug("PutStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, failed_put, Num_w0]),
    {keep_state, State};
waiting(state_timeout, hard_stop, State = #state{req_id = ReqId, from = From}) ->
    logger:debug("PutStatem at WAITING state with event ~p:~p", [state_timeout, hard_stop]),
    From ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(EventType, EventContent, State = #state{}) ->
    logger:debug("PutStatem at WAITING state with event ~p:~p", [EventType, EventContent]),
    {keep_state, State}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
