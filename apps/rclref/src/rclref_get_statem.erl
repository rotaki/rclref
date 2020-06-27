-module(rclref_get_statem).

-behaviour(gen_statem).

-export([get/3]).
-export([start_link/1, stop/2]).
-export([done_get/2, failed_get/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).
-export([reqid/0]).

%TODO: get these numbers from config
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
         values = [] :: [term()],
         preflist :: [term()],
         num_r = 0 :: non_neg_integer(),
         num_w = 0 :: non_neg_integer()}).

% Call the supervisor to start the statem
-spec get(Client :: node(), Key :: riak_object:key(), Options :: [term()]) -> {ok,
                                                                               ReqId ::
                                                                                   non_neg_integer()}.
get(Client, Key, Options) ->
    ReqId = reqid(),
    {ok, _} = rclref_get_statem_sup:start_get_statem([ReqId, self(), Client, Key, Options]),
    {ok, ReqId}.

% Start and Stop
-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, From, Client, Key, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key, Options], []).

-spec stop(pid(), any()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec done_get(pid(), riak_object:value()) -> ok.
done_get(Pid, Value) ->
    gen_statem:cast(Pid, {done_get, Value}).

-spec failed_get(pid()) -> ok.
failed_get(Pid) ->
    gen_statem:cast(Pid, failed_get).

% Callbacks
init([ReqId, From, Client, Key, Options]) ->
    logger:info("Initializing GetStatem, Pid:~p", [self()]),
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
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
    {ok, waiting, State, [{state_timeout, Timeout, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    logger:info("Terminating GetStatem, Pid:~p, Reason:~p", [self(), Reason]),
    ok.

% State function
waiting(cast,
        {done_get, Value},
        State = #state{from = From, req_id = ReqId, num_r = Num_r0, values = Values0}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, done_get, Num_r0]),
    Num_r = Num_r0 + 1,
    Values = Values0 ++ [Value],
    NewState = State#state{num_r = Num_r, values = Values},
    case Num_r =:= ?R of
      true ->
          %TODO: Next state for read repair
          From ! {ReqId, Values},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(cast, failed_get, State = #state{num_r = Num_r0}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p, at num_w: ~p",
                 [cast, failed_get, Num_r0]),
    {keep_state, State};
waiting(state_timeout, hard_stop, State = #state{req_id = ReqId, from = From}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p", [state_timeout, hard_stop]),
    From ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(EventType, EventContent, State = #state{}) ->
    logger:debug("GetStatem at WAITING state with event ~p:~p", [EventType, EventContent]),
    {keep_state, State}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
