-module(rclref_put_statem).

-behaviour(gen_statem).

-export([start_link/1, stop/2]).
-export([result_of_put/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_PUT, rclref_config:timeout_put()).

-record(state,
        {req_id :: non_neg_integer(),
         client_pid :: pid(),
         client_node :: node(),
         preflist :: [term()],
         num_ok = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         riak_objects = [] :: [rclref_object:riak_object()]}).

%% Start and Stop
-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, Client_Pid, Client_Node, RObj, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, Client_Pid, Client_Node, RObj, Options], []).

-spec stop(pid(), term()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec result_of_put(pid(), {ok, ok} | {error, vnode_error}) -> ok.
result_of_put(Pid, Result) ->
    gen_statem:cast(Pid, Result).

% Callbacks
init([ReqId, Client_Pid, Client_Node, RObj, Options]) ->
    logger:info("Initializing PutStatem, Pid:~p", [self()]),
    Key = rclref_object:key(RObj),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    TimeoutPut = proplists:get_value(timeout_put, Options, ?TIMEOUT_PUT),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State =
        #state{req_id = ReqId,
               client_pid = Client_Pid,
               client_node = Client_Node,
               preflist = PrefList},
    lists:foreach(fun ({IndexNode, _}) ->
                          riak_core_vnode_master:command(IndexNode,
                                                         {kv_put_request, RObj, self(), node()},
                                                         rclref_vnode_master)
                  end,
                  PrefList),
    {ok, waiting, State, [{state_timeout, TimeoutPut, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    logger:info("Terminating PutStatem, Pid:~p, Reason:~p", [self(), Reason]),
    ok.

% State function
waiting(cast,
        {ok, ok},
        State = #state{req_id = ReqId, client_pid = Client_Pid, num_ok = Num_ok0}) ->
    Num_ok = Num_ok0 + 1,
    NewState = State#state{num_ok = Num_ok},
    case Num_ok >= ?W of
      true ->
          Client_Pid ! {ReqId, ok},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(cast,
        {error, vnode_error},
        State =
            #state{req_id = ReqId, client_pid = Client_Pid, num_vnode_error = Num_vnode_error0}) ->
    Num_vnode_error = Num_vnode_error0 + 1,
    NewState = State#state{num_vnode_error = Num_vnode_error},
    case Num_vnode_error > ?N - ?W of
      true ->
          Client_Pid ! {ReqId, {error, vnode_error}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(state_timeout,
        hard_stop,
        State = #state{req_id = ReqId, client_pid = Client_Pid}) ->
    Client_Pid ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.
