-module(rclref_put_statem_sup).

-behaviour(supervisor).

-export([start_put_statem/1, stop_put_statem/1, start_link/0]).
-export([init/1]).

start_put_statem(Args) ->
    ReqId = reqid(),
    {ok, _} = supervisor:start_child(?MODULE, [[ReqId] ++ Args]),
    {ok, ReqId}.

stop_put_statem(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok = supervisor:delete_child(?MODULE, Pid).

start_link() ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init([]) ->
    PutStatem =
        {undefined,
         {rclref_put_statem, start_link, []},
         temporary,
         5000,
         worker,
         [rclref_put_statem]},

    {ok, {{simple_one_for_one, 10, 10}, [PutStatem]}}.

% Internal Functions
-spec reqid() -> non_neg_integer().
reqid() ->
    erlang:phash2(erlang:monotonic_time()).
