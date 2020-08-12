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
         num_not_found = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         riak_objects = [] :: [rclref_object:riak_obejct()]}).

-spec start_link([term()]) -> {ok, pid()}.
start_link([ReqId, Client_Pid, Client_Node, Key, Options]) ->
    gen_statem:start_link(?MODULE, [ReqId, Client_Pid, Client_Node, Key, Options], []).

-spec stop(pid(), any()) -> ok.
stop(Pid, Reason) ->
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
-spec result_of_get(pid(),
                    {ok, rclref_object:riak_object()} |
                    {error, not_found} |
                    {error, vnode_error}) ->
                       ok.
result_of_get(Pid, Result) ->
    gen_statem:cast(Pid, Result).

% Callbacks
init([ReqId, Client_Pid, Client_Node, Key, Options]) ->
    logger:info("Initializing GetStatem, Pid:~p", [self()]),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    TimeoutGet = proplists:get_value(timeout_get, Options, ?TIMEOUT_GET),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State =
        #state{req_id = ReqId,
               client_pid = Client_Pid,
               client_node = Client_Node,
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

% waiting for results from vnodes
waiting(cast,
        {ok, RObj},
        State =
            #state{client_pid = Client_Pid,
                   req_id = ReqId,
                   num_ok = Num_ok0,
                   riak_objects = RObjs0}) ->
    Num_ok = Num_ok0 + 1,
    RObjs = RObjs0 ++ [RObj],
    NewState = State#state{num_ok = Num_ok, riak_objects = RObjs},
    case Num_ok >= ?R of
      true ->
          %TODO: Next state for read repair
          Client_Pid ! {ReqId, {ok, RObjs}},
          {next_state, finalize, NewState, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
      false ->
          {keep_state, NewState}
    end;
waiting(cast,
        {error, not_found},
        State =
            #state{client_pid = Client_Pid,
                   req_id = ReqId,
                   num_not_found = Num_not_found0,
                   num_vnode_error = Num_vnode_error0}) ->
    Num_not_found = Num_not_found0 + 1,
    NewState = State#state{num_not_found = Num_not_found},
    Reason =
        case Num_not_found >= Num_vnode_error0 of
          true ->
              not_found;
          false ->
              vnode_error
        end,
    case Num_not_found + Num_vnode_error0 > ?N - ?R of
      true ->
          Client_Pid ! {ReqId, {error, Reason}},
          {next_state, finalize, NewState, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
      false ->
          {keep_state, NewState}
    end;
waiting(cast,
        {error, vnode_error},
        State =
            #state{client_pid = Client_Pid,
                   req_id = ReqId,
                   num_not_found = Num_not_found0,
                   num_vnode_error = Num_vnode_error0}) ->
    Num_vnode_error = Num_vnode_error0 + 1,
    NewState = State#state{num_vnode_error = Num_vnode_error},
    Reason =
        case Num_not_found0 >= Num_vnode_error of
          true ->
              not_found;
          false ->
              vnode_error
        end,
    case Num_not_found0 + Num_vnode_error > ?N - ?R of
      true ->
          Client_Pid ! {ReqId, {error, Reason}},
          {next_state, finalize, NewState, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
      false ->
          {keep_state, NewState}
    end;
waiting(state_timeout,
        hard_stop,
        State = #state{req_id = ReqId, client_pid = Client_Pid}) ->
    Client_Pid ! {ReqId, {error, timeout}},
    {next_state, finalize, State, [{state_timeout, ?TIMEOUT_REPAIR, hard_stop}]};
waiting(_EventType, _EventContent, State = #state{}) ->
    {keep_state, State}.

% read repair
finalize(cast,
         {ok, RObj},
         State =
             #state{num_ok = Num_ok0,
                    num_not_found = Num_not_found0,
                    num_vnode_error = Num_vnode_error0,
                    riak_objects = RObjs0}) ->
    Num_ok = Num_ok0 + 1,
    RObjs = [RObj] ++ RObjs0,
    NewState = State#state{num_ok = Num_ok, riak_objects = RObjs},
    case Num_ok + Num_not_found0 + Num_vnode_error0 >= ?N of
      true ->
          MergedRObj = rclref_object:merge(RObjs),
          ok = repair(MergedRObj, RObjs),
          {stop, normal, NewState};
      false ->
          {keep_state, NewState}
    end;
finalize(cast,
         {error, not_found},
         State =
             #state{num_ok = Num_ok0,
                    num_not_found = Num_not_found0,
                    num_vnode_error = Num_vnode_error0,
                    riak_objects = RObjs0}) ->
    Num_not_found = Num_not_found0 + 1,
    NewState = State#state{num_not_found = Num_not_found},
    case Num_ok0 + Num_not_found + Num_vnode_error0 >= ?N of
      true ->
          case RObjs0 of
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
finalize(cast,
         {error, vnode_error},
         State =
             #state{num_ok = Num_ok0,
                    num_not_found = Num_not_found0,
                    num_vnode_error = Num_vnode_error0,
                    riak_objects = RObjs0}) ->
    Num_vnode_error = Num_vnode_error0 + 1,
    NewState = State#state{num_vnode_error = Num_vnode_error},
    case Num_ok0 + Num_not_found0 + Num_vnode_error >= ?N of
      true ->
          case RObjs0 of
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

repair(RObj, RObjs) ->
    Key = rclref_object:key(RObj),
    Content = rclref_object:content(RObj),
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
