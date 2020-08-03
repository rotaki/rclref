-module(rclref_get_statem_sup).

-behaviour(supervisor).

-export([start_get_statem/1, stop_get_statem/1, start_link/0]).
-export([init/1]).

-spec start_get_statem([term()]) -> {ok, undefined} | {ok, pid()}.
start_get_statem(Args) ->
    {ok, _} = supervisor:start_child(?MODULE, [Args]).

-spec stop_get_statem(pid()) -> ok.
stop_get_statem(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid),
    ok = supervisor:delete_child(?MODULE, Pid).

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, _} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Callbacks
init([]) ->
    GetStatem =
        {undefined,
         {rclref_get_statem, start_link, []},
         temporary,
         5000,
         worker,
         [rclref_put_statem]},
    {ok, {{simple_one_for_one, 10, 10}, [GetStatem]}}.
