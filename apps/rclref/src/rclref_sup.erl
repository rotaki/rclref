-module(rclref_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init(_Args) ->
    VMaster =
        {rclref_vnode_master,
         {riak_core_vnode_master, start_link, [rclref_vnode]},
         permanent,
         5000,
         worker,
         [riak_core_vnode_master]},
    PutStatem =
        {rclref_put_statem_sup,
         {rclref_put_statem_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_put_statem_sup]},
    GetStatem =
        {rclref_get_statem_sup,
         {rclref_get_statem_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_get_statem_sup]},
    CoverageFsm =
        {rclref_coverage_fsm_sup,
         {rclref_coverage_fsm_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rclref_coverage_fsm_sup]},
    {ok, {{one_for_one, 5, 10}, [VMaster, PutStatem, GetStatem, CoverageFsm]}}.
