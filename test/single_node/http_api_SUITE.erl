-module(http_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([http_api_test/1]).

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
    [_Node] = ?config(nodes, Config),
    [Port] = ?config(ports, Config),
    HttpPort = Port + 1,
    Keys = ["key--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    Values = ["value--" ++ integer_to_list(Num) || Num <- lists:seq(1, 20)],
    URLs = [list_to_binary("http://localhost:"  ++ integer_to_list(HttpPort) ++ "/rclref/" ++ Key) || Key <- Keys],

    % check not_found
    lists:foreach(fun(URL) ->
                          {ok, 404, _, ClientRef} = hackney:request(get, URL, [], <<>>, []),
                          {ok, Response} = hackney:body(ClientRef),
                          404 = maps:get(<<"code">>, maps:get(<<"error">>, jsx:decode(Response))),
                          <<"not_found">> = maps:get(<<"reason">>, maps:get(<<"error">>, jsx:decode(Response)))
                  end, URLs),
    
    % put values
    lists:foreach(fun({URL, Value}) ->
                          {ok, 200, _, ClientRef} = hackney:request(post, URL, [], Value, []),
                          {ok, Response} = hackney:body(ClientRef),
                          200 = maps:get(<<"code">>, maps:get(<<"ok">>, jsx:decode(Response)))
                  end, lists:zip(URLs, Values)),
    
    % confirm values
    lists:foreach(fun({URL, Value}) ->
                          {ok, 200, _, ClientRef} = hackney:request(get, URL, [], <<>>, []),
                          {ok, Response} = hackney:body(ClientRef),
                          200 = maps:get(<<"code">>, maps:get(<<"ok">>, jsx:decode(Response))),
                          GotValues = maps:get(<<"values">>, maps:get(<<"ok">>, jsx:decode(Response))),
                          true = lists:all(fun(GotValue) -> list_to_binary(Value) =:= GotValue end, GotValues)
                  end, lists:zip(URLs, Values)),
    
    % delete values
    lists:foreach(fun(URL) ->
                          {ok, 200, _, ClientRef} = hackney:request(delete, URL, [], <<>>, []),
                          {ok, Response} = hackney:body(ClientRef),
                          200 = maps:get(<<"code">>, maps:get(<<"ok">>, jsx:decode(Response)))
                  end, URLs),
    
    % confirm not_found
    lists:foreach(fun(URL) ->
                          {ok, 404, _, ClientRef} = hackney:request(get, URL, [], <<>>, []),
                          {ok, Response} = hackney:body(ClientRef),
                          404 = maps:get(<<"code">>, maps:get(<<"error">>, jsx:decode(Response))),
                          <<"not_found">> = maps:get(<<"reason">>, maps:get(<<"error">>, jsx:decode(Response)))
                  end, URLs),
    ok.
