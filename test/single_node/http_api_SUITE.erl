-module(http_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([http_api_test/1]).

-define(N, rclref_config:n_val()).

all() ->
    [http_api_test].

init_per_suite(Config) ->
    application:ensure_all_started(rclref),
    Names = [node1],
    Ports = [30200],
    Nodes = node_utils:set_up_nodes(Names, Ports, [{module, ?MODULE}]),
    [{module, ?MODULE}, {names, Names}, {nodes, Nodes}, {ports, Ports} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    node_utils:kill_nodes(Nodes),
    Config.

http_api_test(Config) ->
    [Node] = ?config(nodes, Config),
    [Port] = ?config(ports, Config),
    HttpPort = Port + 1,
    Key0 = "key--0",
    Value0 = "value--0",
    URLstring = "http://localhost:"  ++ integer_to_list(HttpPort) ++ "/rclref/",
    URL0 = list_to_binary(URLstring ++ Key0),

    % put "key--0", "value--0"
    {ok, 200, _, ClientRef} = hackney:request(post, URL0, [], Value0, []),
    {ok, <<>>} = hackney:body(ClientRef),

    % get "key--0"
    {ok, 200, _, ClientRef0} = hackney:request(get, URL0, [], <<>>, []),
    {ok, Response0} = hackney:body(ClientRef0),
    Items = maps:get(<<"items">>, jsx:decode(Response0)),
    lists:all(fun(Item) ->
                      ?assertEqual(list_to_binary(Key0), maps:get(<<"key">>, Item)),
                      ?assertEqual(list_to_binary(Value0), maps:get(<<"value">>, Item)),
                      ?assertEqual(atom_to_binary(Node), maps:get(<<"node">>, Item)),
                      true
              end,
              Items),
    
    % get "key--1" (error)
    Key1 = "key--1",
    URL1 = list_to_binary(URLstring ++ Key1),
    {ok, 500, _, ClientRef1} = hackney:request(get, URL1, [], <<>>, []),
    {ok, Response1} = hackney:body(ClientRef1),
    <<"not_found">> = maps:get(<<"reason">>, maps:get(<<"error">>, jsx:decode(Response1))),
    ok.


    
atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

    
