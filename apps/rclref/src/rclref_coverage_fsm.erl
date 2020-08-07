-module(rclref_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-export([start_link/1]).
-export([init/2, process_results/2, finish/2]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).

-record(state, {req_id, client_pid, request, accum = []}).

start_link([ReqId, Client_Pid, _Client_Node, Request, Options]) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_COVERAGE),
    riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, Client_Pid}, [Request, Timeout]).

% Callbacks
% Client_Pid: Pid, Client_Node: Node
init({pid, ReqId, Client_Pid}, [Request = {unique, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, client_pid = Client_Pid, request = Request, accum = []},
    {Request, allup, ?N, 1, rclref, rclref_vnode_master, Timeout, State};
init({pid, ReqId, Client_Pid}, [Request = {all, _}, Timeout]) ->
    logger:info("Initializing CoverageFsm, Pid: ~p", [self()]),
    State = #state{req_id = ReqId, client_pid = Client_Pid, request = Request, accum = []},
    {Request, allup, ?N, ?N, rclref, rclref_vnode_master, Timeout, State}.

process_results({{_ReqId, {_Partition, _Node}}, []}, State) ->
    {done, State};
process_results({{_ReqId, {_Partition, _Node}}, Data}, State = #state{accum = Accum}) ->
    % If you need to get which partition and node the data comes from
    % NewAccum = [{Partition, Node, Data} |  Accum],
    NewAccum = Data ++ Accum,
    {done, State#state{accum = NewAccum}}.

finish(clean,
       State =
           #state{req_id = ReqId, request = {unique, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    NewAccum = lists:usort(Accum),
    Client_Pid ! {ReqId, {ok, NewAccum}},
    {stop, normal, State};
finish(clean,
       State =
           #state{req_id = ReqId, request = {all, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:info("Terminating CoverageFsm, Pid: ~p", [self()]),
    Client_Pid ! {ReqId, {ok, Accum}},
    {stop, normal, State};
finish({error, Reason},
       State =
           #state{req_id = ReqId, request = {unique, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    NewAccum = lists:usort(Accum),
    Client_Pid ! {ReqId, {partial, Reason, NewAccum}},
    {stop, normal, State};
finish({error, Reason},
       State =
           #state{req_id = ReqId, request = {all, _}, client_pid = Client_Pid, accum = Accum}) ->
    logger:error("Coverage query failed! Reason: ~p", [Reason]),
    Client_Pid ! {ReqId, {partial, Reason, Accum}},
    {stop, normal, State}.
