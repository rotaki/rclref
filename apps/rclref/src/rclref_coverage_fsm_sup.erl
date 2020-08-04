-module(rclref_coverage_fsm_sup).

-behaviour(supervisor).

-export([start_link/0, start_coverage_statem/1, stop_coverage_statem/1]).
-export([init/1]).

start_coverage_statem(Args) ->
    {ok, _} =  supervisor:start_child(?MODULE, [Args]).

stop_coverage_statem(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok = supervisor:delete_child(?MODULE, Pid).
    
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    CoverageFsm = {undefined,
                      {rclref_coverage_fsm, start_link, []},
                      temporary, 
                      5000, 
                      worker, 
                      [rclref_coverage_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [CoverageFsm]}}.
