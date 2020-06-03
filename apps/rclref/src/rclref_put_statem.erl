-module(rclref_put_statem).

-behaviour(gen_statem).

-export([put/3]).
-export([start_link/1, stop/2]).
-export([init/1, terminate/3]).
-export([done_put/1, fail_put/1]).
-export([reqid/0]).

-define(N, 1).
-define(W, 1).
-define(R, 1).

-record(state, {req_id, from, client, key, value, preflist, num_r = 0, num_w = 0}).

%% Call the supervisor to start the statem
put(Client, Key, Value) ->
    ReqId = reqid(),
    rclref_put_statem_sup:start_put_statem([ReqId, self(), Client, Key, Value]),
    {ok, ReqId}.

%% Start and Stop
start_link([ReqId, From, Client, Key, Value]) ->
    io:format("(start_link) starting put statem~n"),
    gen_statem:start_link(?MODULE, [ReqId, From, Client, Key, Value], []).

stop(Pid, Reason) ->
    io:format("(stop) stopping put state machine"),
    gen_statem:stop(Pid, Reason, infinity).

% API (called by vnodes)
done_put(Pid) ->
    gen_statem:cast(Pid, {ok, done_put}).

fail_put(Pid) ->
    gen_statem:cast(Pid, {error, fail_put}).

% Callbacks
init([ReqId, From, Client, Key, Value]) ->
    io:format("statem:init~n"),
    DocIdx = riak_core_util:chashkey({list_to_binary(Client), list_to_binary(Key)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, ?N, rclref),
    State = #state{req_id = ReqId,
                   from = From,
                   client = Client,
                   key = Key,
                   value = Value,
                   preflist = PrefList},
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:command(IndexNode,
                                   {kv_put_request, Key, Value, self()},
                                   rclref_vnode_master),

    {ok, waiting, State}.

terminate(_Reason, _StateName, _State) ->
    io:format("(terminate) terminating put statem~n"),
    ok.

%% State function
waiting(cast,
        {ok, done_put},
        State = #state{req_id = ReqId, from = From, num_w = Num_w0}) ->
    Num_w = Num_w0 + 1,
    NewState = State#state{num_w = Num_w},
    case Num_w =:= ?W of
      true ->
          From ! {ReqId, ok},
          %TODO: stop
          {stop, ok};
      false ->
          {keep_state, NewState}
    end;
waiting(cast, {error, fail_put}, State) ->
    {keep_state, State}.

% Internal Functions
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
