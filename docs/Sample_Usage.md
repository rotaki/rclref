# Usage

## Erlang interface

Get a Key-Value with key = dog

```erlang
(rclref@127.0.0.1)1> rclref_client:get(<<"dog">>).
{error,not_found}
```

Store a Key-Value with key = dog, value = cat


```erl
(rclref@127.0.0.1)2> rclref_client:put(<<"dog">>, <<"cat">>).
ok
```

Get a Key-Value with key = dog

```erl
(rclref@127.0.0.1)7> rclref_client:get(<<"dog">>).
{ok,[<<"cat">>,<<"cat">>,<<"cat">>]}
```

List all keys

```erl
(rclref@127.0.0.1)2> rclref_client:list_keys().
{ok,[<<"dog">>]}
```

Delete a Key-Value with key = dog. (Internally, this will not delete the value but leave it as a tombstone)

```erl
(rclref@127.0.0.1)8> rclref_client:delete(<<"dog">>).
ok
```

Get a Key-Value with key = dog. Note that tombstones are not observable.

```erl
(rclref@127.0.0.1)8> rclref_client:get(<<"dog">>).
{error,not_found}
```


## HTTP interface

Get a Key-Value with key = dog

```sh
| => curl -X GET http://localhost:8080/rclref/dog
{
  "error": {
    "code": 404,
    "reason": "not_found"
  }
}
```

Store a Key-Value with key = dog, value = cat

```sh
| => curl -X POST http://localhost:8080/rclref/dog -d 'cat'
{
  "ok": {
    "code": 200
  }
}
```

Get a Key-Value with key = dog

```sh
| => curl -X GET http://localhost:8080/rclref/dog
{
  "ok": {
    "code": 200,
    "values": [
      "cat",
      "cat",
      "cat"
    ]
  }
}
```

Delete a Key-Value with key = dog. (Internally, this will not delete the value but leave it as a tombstone)

```sh
| => curl -X DELETE http://localhost:8080/rclref/dog
{
  "ok": {
    "code": 200
  }
}
```

Get a Key-Value with key = dog. Note that tombstones are not observable.

```sh
| => curl -X GET http://localhost:8080/rclref/dog
{
  "error": {
    "code": 404,
    "reason": "not_found"
  }
}
```

