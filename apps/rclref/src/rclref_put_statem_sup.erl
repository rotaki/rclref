-module(rclref_put_statem_sup).

-behaviour(supervisor).

-export([start_put_statem/1, start_link/0]).
-export([init/1]).

start_put_statem(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                %          callbacks          %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    PutStatem = {undefined,
                 {rclref_put_statem, start_link, []},
                 temporary,
                 5000,
                 worker,
                 [rclref_put_statem]},

    {ok, {{simple_one_for_one, 10, 10}, [PutStatem]}}.
