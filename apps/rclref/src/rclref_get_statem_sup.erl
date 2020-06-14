-module(rclref_get_statem_sup).

-behaviour(supervisor).

-export([start_get_statem/1, stop_get_statem/1, start_link/0]).
-export([init/1]).

start_get_statem(Args) ->
    {ok, _} = supervisor:start_child(?MODULE, [Args]).

stop_get_statem(Pid) ->
    {ok, _} = supervisor:terminate_child(?MODULE, Pid),
    {ok, _} = supervisor:delete_child(?MODULE, Pid).

start_link() ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init([]) ->
    GetStatem = {undefined,
                 {rclref_get_statem, start_link, []},
                 temporary,
                 5000,
                 worker,
                 [rclref_put_statem]},
    {ok, {{simple_one_for_one, 10, 10}, [GetStatem]}}.
