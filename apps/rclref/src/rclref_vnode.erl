-module(rclref_vnode).

-behaviour(riak_core_vnode).

-export([start_vnode/1, init/1, terminate/2, handle_command/3, is_empty/1, delete/1,
         handle_handoff_command/3, handoff_starting/2, handoff_cancelled/1, handoff_finished/2,
         handle_handoff_data/2, encode_handoff_item/2, handle_overload_command/3,
         handle_overload_info/2, handle_coverage/4, handle_exit/3]).

-ignore_xref([{start_vnode, 1}]).

-record(state, {index, mod, modstate}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Index]) ->
    %TODO: Get model from config
    Mod = rclref_ets_backend,
    {ok, ModState} = Mod:start(Index, undefined),
    logger:debug("Successfully started ~p backend for index ~p", [Mod, Index]),
    State = #state{index = Index, mod = Mod, modstate = ModState},
    {ok, State}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, node(), State#state.index}, State};
handle_command({kv_put_request, Key, Value, Pid},
               _Sender,
               State0 = #state{index = Index, mod = Mod, modstate = ModState0}) ->
    case Mod:put(Key, Value, ModState0) of
      {ok, ModState1} ->
          logger:debug("Successfully put kv with key: ~p, value: ~p for index: ~p",
                       [Key, Value, Index]),
          rclref_put_statem:done_put(Pid),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1};
      {error, Reason, ModState1} ->
          logger:error("Failed to put kv with key: ~p, value: ~p for index: ~p, error: ~p",
                       [Key, Value, Index, Reason]),
          rclref_put_statem:failed_put(Pid),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1}
    end;
handle_command({kv_get_request, Key, Pid},
               _Sender,
               State0 = #state{index = Index, mod = Mod, modstate = ModState0}) ->
    case Mod:get(Key, ModState0) of
      {ok, not_found, ModState1} ->
          logger:debug("Failed to get kv with key: ~p for index: ~p, error: ~p",
                       [Key, Index, not_found]),

          rclref_get_statem:failed_get(Pid),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1};
      {ok, Value, ModState1} ->
          logger:debug("Successfully put kv with key: ~p for index: ~p", [Key, Index]),
          RObj = rclref_object:new(Key, Value),
          rclref_get_statem:done_get(Pid, RObj),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1};
      {error, Reason, ModState1} ->
          logger:error("Failed to get kv with key: ~p for index: ~p, error: ~p",
                       [Key, Index, Reason]),
          rclref_get_statem:failed_get(Pid),
          State1 = State0#state{modstate = ModState1},
          {noreply, State1}
    end;
handle_command(Message, _Sender, State) ->
    logger:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

handle_overload_command(_, _, _) ->
    ok.

handle_overload_info(_, _Idx) ->
    ok.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
