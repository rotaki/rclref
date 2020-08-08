-module(rclref_http_handler).

-export([init/2]).

init(ReqIn = #{method := <<"POST">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    {ok, Value, Req1} = read_all_body(ReqIn),
    RObj = rclref_object:new(Key, Value),
    ReqOut =
        case rclref:put(RObj) of
          ok ->
              cowboy_req:reply(200, Req1);
          Error = {error, _} ->
              Data = error_to_map(Error),
              EncodedData = jsx:encode(Data, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedData,
                               ReqIn)
        end,
    {ok, ReqOut, State};
init(ReqIn = #{method := <<"DELETE">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    ReqOut =
        case rclref:delete(Key) of
          ok ->
              cowboy_req:reply(200, ReqIn);
          Error = {error, _} ->
              Data = error_to_map(Error),
              EncodedData = jsx:encode(Data, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedData,
                               ReqIn)
        end,
    {ok, ReqOut, State};
init(ReqIn = #{method := <<"GET">>}, State) ->
    Key = cowboy_req:binding(key, ReqIn),
    ReqOut =
        case rclref:get(Key) of
          {ok, RObjs} ->
              Items = [robj_to_map(RObj) || RObj <- RObjs],
              Data = #{kind => rclref_objects, items => Items},
              EncodedData = jsx:encode(Data, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(200,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedData,
                               ReqIn);
          Error = {error, _} ->
              Data = error_to_map(Error),
              EncodedData = jsx:encode(Data, [{space, 1}, {indent, 2}]),
              cowboy_req:reply(500,
                               #{<<"content-type">> => <<"application/json">>},
                               EncodedData,
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

robj_to_map(RObj) ->
    #{kind => rclref_object,
      key => rclref_object:key(RObj),
      value => rclref_object:value(RObj),
      partition => integer_to_binary(rclref_object:partition(RObj)),
      node => rclref_object:node(RObj)}.

error_to_map({error, Reason}) ->
    #{error => #{reason => Reason}, code => 500}.
