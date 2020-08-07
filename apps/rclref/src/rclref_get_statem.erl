-module(rclref_get_statem).

-behaviour(gen_statem).

-export([start_link/1, stop/2]).
-export([result_of_get/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([waiting/3]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_GET, rclref_config:timeout_get()).

-record(state,
        {req_id :: non_neg_integer(),
         client_pid :: pid(),
         client_node :: node(),
         preflist :: [term()],
         num_ok = 0 :: non_neg_integer(),
         num_not_found = 0 :: non_neg_integer(),
         num_vnode_error = 0 :: non_neg_integer(),
         riak_objects :: [rclref_object:riak_obejct()]}).

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
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_GET),
    DocIdx = riak_core_util:chash_key({Key, undefined}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State =
        #state{req_id = ReqId,
               client_pid = Client_Pid,
               client_node = Client_Node,
               preflist = PrefList,
               riak_objects = []},
    Fn =
        fun (IndexNode) ->
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
          {stop, normal, NewState};
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
          {stop, normal, NewState};
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
