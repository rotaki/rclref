-module(rclref_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-export([coverage/3]).
-export([start_link/1]).
-export([init/2, process_results/2, finish/2]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).

-record(state, {req_id, from, request, accum = []}).

coverage(Client, Request, Options) ->
    ReqId = reqid(),
    {ok, _} =
        rclref_coverage_fsm_sup:start_coverage_fsm([ReqId, self(), Client, Request, Options]),
    {ok, ReqId}.

start_link([ReqId, From, _Client, Request, Options]) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_COVERAGE),
    riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, From}, [Request, Timeout]).

% Callbacks
% From: Pid, Client: Node
init({pid, ReqId, From}, [Request = {unique, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, from = From, request = Request, accum = []},
    {Request, allup, ?N, 1, rclref, rclref_vnode_master, Timeout, State};
init({pid, ReqId, From}, [Request = {all, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, from = From, request = Request, accum = []},
    {Request, allup, ?N, ?N, rclref, rclref_vnode_master, Timeout, State}.

process_results({{_ReqId, {_Partition, _Node}}, []}, State) ->
    {done, State};
process_results({{_ReqId, {_Partition, _Node}}, Data}, State = #state{accum = Accum}) ->
    % If you need to get which partition and node the data comes from
    % NewAccum = [{Partition, Node, Data} |  Accum],
    NewAccum = Data ++ Accum,
    {done, State#state{accum = NewAccum}}.

finish(clean,
       State = #state{req_id = ReqId, request = {unique, _}, from = From, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    NewAccum = lists:usort(Accum),
    From ! {ReqId, {ok, NewAccum}},
    {stop, normal, State};
finish(clean,
       State = #state{req_id = ReqId, request = {all, _}, from = From, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    From ! {ReqId, {ok, Accum}},
    {stop, normal, State};
finish({error, Reason},
       State = #state{req_id = ReqId, request = {unique, _}, from = From, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    NewAccum = lists:usort(Accum),
    From ! {ReqId, {partial, Reason, NewAccum}},
    {stop, normal, State};
finish({error, Reason},
       State = #state{req_id = ReqId, request = {all, _}, from = From, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    From ! {ReqId, {partial, Reason, Accum}},
    {stop, normal, State}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
