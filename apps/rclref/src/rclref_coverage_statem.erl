-module(rclref_coverage_statem).

-behaviour(riak_core_coverage_statem).

-export([coverage/3]).
-export([start_link/1, stop/2]).
-export([init/2, process_results/2, finish/2]).

-define(N, rclref_config:n_val()).
-define(W, rclref_config:w_val()).
-define(R, rclref_config:r_val()).
-define(TIMEOUT_COVERAGE, rclref_config:timeout_coverage()).

coverage(Client, Command, Options) ->
    ReqId = reqid(),
    {ok, _} = rclref_coverage_statem_sup:start_coverage_statem([ReqId, self(), Client, Command, Options]),
    {ok, ReqId}.

start_link([ReqId, From, _Client, Command, Options]) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT_COVERAGE),
    riak_core_coverage_statem:start_link(?MODULE, {pid, ReqId, From}, [Command, Timeout]).

stop(Pid, Reason) ->
    gen_statem:cast(Pid, Reason, infinity).

% Callbacks
% From: Pid, Client: Node
init({pid, ReqId, From}, [Command, Timeout]) ->
    logger:info("Initializing CoverageStatem, Pid: ~p", [self()]),
    State = #{req_id => ReqId, from => From, request => Command, accum => []},
    {Command, allup, ?N, 1, rclref, rclref_vnode_master, Timeout, State}.

process_results({{_ReqId, {_Partition, _Node}}, []}, State) ->
    {done, State};
process_results({{_ReqId, {Partition, Node}}, Data}, State = #{accum := Accum}) ->
    NewAccum = [{Partition, Node, Data} |  Accum],
    {done, State#{accum => NewAccum}}.

finish(clean, State = #{req_id := ReqId, from := From, accum := Accum}) ->
    logger:info("Finished coverage request ~p", [ReqId]),
    From ! {ReqId, {ok, Accum}},
    {stop, normal, State};
finish({error, Reason}, State = #{req_id := ReqId, from := From, accum := Accum}) ->
    logger:warning("Coverage query failed! Reason: ~p", [Reason]),
    From ! {ReqId, {partial, Reason, Accum}},
    {stop, normal, State}.


% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
