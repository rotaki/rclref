-module(rclref_get_statem).

-behaviour(gen_statem).

-export([start_link/1, stop/2]).
-export([result_of_get/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3, finalize/3]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_GET, rclref_config:timeout_get()).
-define(TIMEOUT_REPAIR, 10000).

-record(state,
        {req_id :: non_neg_integer(),
         client_pid :: pid(),
         client_node :: node(),
         preflist :: [term()],
         num_ok = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         vnode_errors = [] :: [rclref_object:vnode_error()],
         riak_objects = [] :: [rclref_object:riak_obejct()]}).

-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, ClientPid, ClientNode, Key, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, ClientPid, ClientNode, Key, Options], []).

-spec stop(pid(), any()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec result_of_get(pid(),
                    {ok, rclref_object:riak_object()} | {error, rclref_object:vnode_error()}) ->
                       ok.
result_of_get(Pid, Result) ->
    gen_statem:cast(Pid, Result).

% Callbacks
init([ReqId, ClientPid, ClientNode, Key, Options]) ->
    logger:info("Initializing GetStatem, Pid:~p", [self()]),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    TimeoutGet = proplists:get_value(timeout_get, Options, ?TIMEOUT_GET),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State =
        #state{req_id = ReqId,
               client_pid = ClientPid,
               client_node = ClientNode,
               preflist = PrefList},
    lists:foreach(fun ({IndexNode, _}) ->
                          riak_core_vnode_master:command(IndexNode,
                                                         {kv_get_request, Key, self()},
                                                         rclref_vnode_master)
                  end,
                  PrefList),
    {ok, waiting, State, [{state_timeout, TimeoutGet, hard_stop}]}.

callback_mode() ->
    state_functions.

code_change(_Vsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    logger:info("Terminating GetStatem, Pid:~p, Reason:~p", [self(), Reason]),
    ok.

% WAITING STATE will wait for ?R vnodes to return {ok, RObj}
% When a vnode returns {ok, RObj}
waiting(cast,
        {ok, RObj},
        State =
            #state{client_pid = ClientPid,
                   req_id = ReqId,
                   num_ok = NumOk0,
                   riak_objects = RObjs0}) ->
    % Update State
    NumOk = NumOk0 + 1,
    RObjs = RObjs0 ++ [RObj],
    NewState = State#state{num_ok = NumOk, riak_objects = RObjs},

    % When more than or equal to ?R vnodes responded with {ok, RObj}, return ?R RObjs to client
    case NumOk >= ?R of
      true ->
          ClientPid ! {ReqId, {ok, RObjs}},
          {next_state, finalize, NewState, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
      false ->
          {keep_state, NewState}
    end;
% When a vnode returns {error, VnodeError}
waiting(cast,
        {error, VnodeError},
        State =
            #state{client_pid = ClientPid,
                   req_id = ReqId,
                   num_vnode_error = NumVnodeError0,
                   vnode_errors = VnodeErrors0,
                   riak_objects = RObjs0}) ->
    % Update State
    NumVnodeError = NumVnodeError0 + 1,
    VnodeErrors = [VnodeError] ++ VnodeErrors0,
    NewState = State#state{num_vnode_error = NumVnodeError, vnode_errors = VnodeErrors},

    % When more than (?N-?R) vnodes responded with {error, VnodeError}, return all RObjs and VNodeErrors it has received to client
    case NumVnodeError > ?N - ?R of
      true ->
          ClientPid ! {ReqId, {{ok, RObjs0}, {error, VnodeErrors}}},
          {next_state, finalize, NewState, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
      _ ->
          {keep_state, NewState}
    end;
% When waiting timeouts, go to finalize state
waiting(state_timeout,
        hard_stop,
        State = #state{req_id = ReqId, client_pid = ClientPid}) ->
    ClientPid ! {ReqId, {error, timeout}},
    {next_state, finalize, State, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.

% FINALIZE STATE will wait for ?N vnodes to return response and then issue read_pair
% When a vnode returns {ok, RObj}
finalize(cast,
         {ok, RObj},
         State =
             #state{num_ok = NumOk0, num_vnode_error = NumVnodeError0, riak_objects = RObjs0}) ->
    % Update State
    NumOk = NumOk0 + 1,
    RObjs = [RObj] ++ RObjs0,
    NewState = State#state{num_ok = NumOk, riak_objects = RObjs},

    % When all ?N vnodes has responded, do read_repair
    case NumOk + NumVnodeError0 >= ?N of
      true ->
          MergedRObj = rclref_object:merge(RObjs),
          ok = repair(MergedRObj, RObjs),
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
% When a vnode returns {error, VnodeError}
finalize(cast,
         {error, _VnodeError},
         State =
             #state{num_ok = NumOk0, num_vnode_error = NumVnodeError0, riak_objects = RObjs0}) ->
    % Update Satte
    NumVnodeError = NumVnodeError0 + 1,
    NewState = State#state{num_vnode_error = NumVnodeError},

    % When all ?N vnodes has responded, do read_repair
    case NumOk0 + NumVnodeError >= ?N of
      true ->
          case RObjs0 of
            % When any of the vnodes responded with {ok, RObj}, do not issue read_repair
            [] ->
                {stop, normal, NewState};
            _ ->
                MergedRObj = rclref_object:merge(RObjs0),
                ok = repair(MergedRObj, RObjs0),
                {stop, normal, NewState}
          end;
      false ->
          {keep_state, NewState}
    end;
% When finalize state timeouts, issue a read_repair
finalize(state_timeout, hard_stop, State = #state{riak_objects = RObjs}) ->
    case RObjs of
      [] ->
          {stop, normal, State};
      _ ->
          MergedRObj = rclref_object:merge(RObjs),
          ok = repair(MergedRObj, RObjs),
          {stop, normal, State}
    end;
finalize(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.

% Use RObj to repair vnodes with different content (i.e Value, VClock)
repair(RObj, RObjs) ->
    Key = rclref_object:key(RObj),
    Content = rclref_object:content(RObj),
    % Exclude repairing of vnodes that has the same content
    OkNodesIndexes =
        [{rclref_object:partition(X), rclref_object:node(X)}
         || X <- RObjs, Key =:= rclref_object:key(X), Content =:= rclref_object:content(X)],
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    lists:foreach(fun ({IndexNode, _}) ->
                          case lists:member(IndexNode, OkNodesIndexes) of
                            false ->
                                logger:info("Sending repair RObj: ~p to IndexNode: ~p",
                                            [RObj, IndexNode]),
                                riak_core_vnode_master:command(IndexNode,
                                                               {repair_request, RObj},
                                                               rclref_vnode_master);
                            _ ->
                                ok
                          end
                  end,
                  PrefList),
    ok.
