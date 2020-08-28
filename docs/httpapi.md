# HttpAPI

This page provides an overview of how Http API is implemented in rclref.
Http API is implemented using the cowboy library. Check [`here`](https://github.com/ninenines/cowboy) for details of cowboy.

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.


!!! Note
    Add `cowboy` and `jsx` as the dependency in `rebar.config` and `rclref.app.src`.
    
## Starting Http Server

To start the http server on starting the application, add the following function in the [`rclref_app.erl`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_app.erl) and call it inside `rclref_app:start/2`.

```erlang
setup_http_api() ->
    Dispatch = cowboy_router:compile([{'_', [{"/rclref/:key", rclref_http_handler, []}]}]),
    HttpPort = rclref_config:http_port(),
    HttpAcceptors = rclref_config:http_acceptors(),
    HttpMaxConnections = rclref_config:http_max_connections(),

    logger:info("Starting HTTP API at port ~p", [HttpPort]),

    {ok, _} =
        cowboy:start_clear(rclref_http_listener,
                           [{port, HttpPort},
                            {num_acceptors, HttpAcceptors},
                            {max_connections, HttpMaxConnections}],
                           #{env => #{dispatch => Dispatch}}),

    ok.
```


## Writing the handler

The handler for http request is defined in [`rclref_http_handler`](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref_http_handler.erl). A http request will be converted to a request using `rclref_client` module. Then the response will be converted to a JSON format using the jsx library and sent back to the client.

```erlang
-module(rclref_http_handler).

-export([init/2]).

init(ReqIn = #{method := <<"POST">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    {ok, Value, Req1} = read_all_body(ReqIn),
    ReqOut =
        case rclref_client:put(Key, Value) of
          ok ->
              OK = #{ok => #{code => 200}},
              EncodedOK = jsx:encode(OK, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(200,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedOK,
                               Req1);
          {error, partial} ->
              Error = #{error => #{reason => partial, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               Req1);
          {error, timeout} ->
              Error = #{error => #{reason => timeout, code => 408}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(408,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               Req1);
          {error, Reasons} ->
              Error = #{error => #{reason => Reasons, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               Req1)
        end,
    {ok, ReqOut, State};
init(ReqIn = #{method := <<"DELETE">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    ReqOut =
        case rclref_client:delete(Key) of
          ok ->
              OK = #{ok => #{code => 200}},
              EncodedOK = jsx:encode(OK, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(200,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedOK,
                               ReqIn);
          {error, partial} ->
              Error = #{error => #{reason => partial, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn);
          {error, timeout} ->
              Error = #{error => #{reason => timeout, code => 408}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(408,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn);
          {error, Reasons} ->
              Error = #{error => #{reason => Reasons, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn)
        end,
    {ok, ReqOut, State};
init(ReqIn = #{method := <<"GET">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    ReqOut =
        case rclref_client:get(Key) of
          {ok, Values} ->
              Data = #{ok => #{values => Values, code => 200}},
              EncodedData = jsx:encode(Data, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(200,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedData,
                               ReqIn);
          {error, not_found} ->
              Error = #{error => #{reason => not_found, code => 404}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(404,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn);
          {error, partial} ->
              Error = #{error => #{reason => partial, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn);
          {error, timeout} ->
              Error = #{error => #{reason => timeout, code => 408}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(408,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn);
          {error, Reasons} ->
              Error = #{error => #{reason => Reasons, code => 500}},
              EncodedError = jsx:encode(Error, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedError,
                               ReqIn)
        end,
    {ok, ReqOut, State}.

read_all_body(ReqIn) ->
    read_all_body(ReqIn, <<>>).

read_all_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
      {ok, Data, Req} ->
          {ok, <<Acc/binary, Data/binary>>, Req};
      {more, Data, Req} ->
          read_all_body(Req, <<Acc/binary, Data/binary>>)
    end.
```
