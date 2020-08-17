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
         n_val :: non_neg_integer(),
         w_val :: non_neg_integer(),
         num_ok = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         vnode_errors = [] :: [rclref_object:vnode_error()],
         riak_objects = [] :: [rclref_object:riak_object()]}).

%% Start and Stop
-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, ClientPid, ClientNode, RObj, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, ClientPid, ClientNode, RObj, Options], []).

-spec stop(pid(), term()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec result_of_put(pid(),
                    {ok, rclref_object:riak_object()} | {error, rclref_object:vnode_error()}) ->
                       ok.
result_of_put(Pid, Result) ->
    gen_statem:cast(Pid, Result).

% Callbacks
init([ReqId, ClientPid, ClientNode, RObj, Options]) ->
    logger:info("Initializing PutStatem, Pid:~p", [self()]),
    Key = rclref_object:key(RObj),
    DocIdx = riak_core_util:chash_key({Key, undefined}),

    TimeoutPut = proplists:get_value(timeout, Options, ?TIMEOUT_PUT),
    N = proplists:get_value(n_val, Options, ?N),
    W = proplists:get_value(w_val, Options, ?W),

    logger:error("N ~p", [N]),
    logger:error("W ~p", [W]),

    PrefList = riak_core_apl:get_primary_apl(DocIdx, N, rclref),

    State =
        #state{req_id = ReqId,
               client_pid = ClientPid,
               client_node = ClientNode,
               preflist = PrefList,
               n_val = N,
               w_val = W},

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
% WAITING STATE will wait for the responses from vnodes until
% When a vnode return {ok, RObj}
waiting(cast,
        {ok, RObj},
        State =
            #state{req_id = ReqId,
                   client_pid = ClientPid,
                   w_val = W,
                   num_ok = NumOk0,
                   riak_objects = RObjs0}) ->
    % Update State
    RObjs = [RObj] ++ RObjs0,
    NumOk = NumOk0 + 1,
    NewState = State#state{num_ok = NumOk, riak_objects = RObjs},

    % When more than or equal to W vnodes responded with {ok, RObj}, return W RObjs to client
    case NumOk >= W of
      true ->
          ClientPid ! {ReqId, {ok, RObjs}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
% When a vnode return {error, VnodeError}
waiting(cast,
        {error, VnodeError},
        State =
            #state{req_id = ReqId,
                   client_pid = ClientPid,
                   n_val = N,
                   w_val = W,
                   num_vnode_error = NumVnodeError0,
                   vnode_errors = VnodeErrors0,
                   riak_objects = RObjs0}) ->
    % Update State
    NumVnodeError = NumVnodeError0 + 1,
    VnodeErrors = [VnodeError] ++ VnodeErrors0,
    NewState = State#state{num_vnode_error = NumVnodeError, vnode_errors = VnodeErrors},

    % When more than (N-R) vnodes responded with {error, VnodeError}, return all RObjs and VnodeErrors it has received to client
    case NumVnodeError > N - W of
      true ->
          ClientPid ! {ReqId, {{ok, RObjs0}, {error, VnodeErrors}}},
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
waiting(state_timeout,
        hard_stop,
        State = #state{req_id = ReqId, client_pid = ClientPid}) ->
    ClientPid ! {ReqId, {error, timeout}},
    {stop, waiting_timed_out, State};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.
