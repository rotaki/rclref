# Low Level API

!!! Warning
    Please check out the [repository](https://github.com/wattlebirdaz/rclref) for the latest code.



```plantuml
title rclref_client:get(Key)

[*] --> UserAPI 
UserAPI -down-> LowLevelAPI: get request
LowLevelAPI -down-> Supervisor
Supervisor -down-> Coordinator : simple one for one
Coordinator --> Vnode1
Coordinator --> Vnode2
Coordinator --> Vnode3
Vnode1 --> Coordinator
Vnode2 --> Coordinator
Vnode3 --> Coordinator
Vnode1 -down-> Backend1
Vnode2 -down-> Backend2
Vnode3 -down-> Backend3
Backend1 --> Vnode1
Backend2 --> Vnode2
Backend3 --> Vnode3
Coordinator -right-> LowLevelAPI
LowLevelAPI -up-> UserAPI

UserAPI: rclref_client.erl
LowLevelAPI: rclref.erl
Supervisor: rclref_get_statem_sup.erl
Coordinator: rclref_get_statem.erl
Vnode1: rclref_vnode.erl
Vnode2: rclref_vnode.erl
Vnode3: rclref_vnode.erl
Backend1: rclref_ets_backend.erl
Backend2: rclref_ets_backend.erl
Backend3: rclref_ets_backend.erl
Backend1: rclref_dets_backend.erl
Backend2: rclref_dets_backend.erl
Backend3: rclref_dets_backend.erl
```

## Why we need low level API?

There are 3 APIs in rclref which are UserAPI, HttpAPI, and LowLevelAPI. LowLevelAPI should be used for debugging and the other two should be used for normal usage.
LowLevelAPI will provide a detailed information about the vnode on put, get and delete such as node, parition and current status of the vector clock.

```erlang
(rclref@127.0.0.1)1> RObj = rclref_object:new(<<"dog">>, <<"cat">>).
{r_object,<<"dog">>,
          {r_content,<<"cat">>,#{}},
          undefined,undefined}
          
(rclref@127.0.0.1)2> rclref:put(RObj).
{ok,[{r_object,<<"dog">>,
               {r_content,<<"cat">>,#{'rclref@127.0.0.1' => 1}},
               730750818665451459101842416358141509827966271488,
               'rclref@127.0.0.1'}]}
               
(rclref@127.0.0.1)3> rclref:get(<<"dog">>).
{ok,[{r_object,<<"dog">>,
               {r_content,<<"cat">>,#{'rclref@127.0.0.1' => 1}},
               548063113999088594326381812268606132370974703616,
               'rclref@127.0.0.1'}]}
               
(rclref@127.0.0.1)4> rclref:delete(<<"dog">>).
{ok,[{r_object,<<"dog">>,
               {r_content,undefined,#{'rclref@127.0.0.1' => 2}},
               548063113999088594326381812268606132370974703616,
               'rclref@127.0.0.1'}]}
               
(rclref@127.0.0.1)5> rclref:get(<<"dog">>).
{{ok,[]},
 {error,[{vnode_error,not_found,
                      913438523331814323877303020447676887284957839360,
                      'rclref@127.0.0.1'},
         {vnode_error,not_found,
                      730750818665451459101842416358141509827966271488,
                      'rclref@127.0.0.1'},
         {vnode_error,not_found,
                      548063113999088594326381812268606132370974703616,
                      'rclref@127.0.0.1'}]}}
                      
(rclref@127.0.0.1)9> rclref:list_all_objects().
{ok,[{r_object,<<"dog">>,
               {r_content,undefined,#{'rclref@127.0.0.1' => 2}},
               548063113999088594326381812268606132370974703616,
               'rclref@127.0.0.1'},
     {r_object,<<"dog">>,
               {r_content,undefined,#{'rclref@127.0.0.1' => 2}},
               730750818665451459101842416358141509827966271488,
               'rclref@127.0.0.1'},
     {r_object,<<"dog">>,
               {r_content,undefined,#{'rclref@127.0.0.1' => 2}},
               913438523331814323877303020447676887284957839360,
               'rclref@127.0.0.1'}]}
               
(rclref@127.0.0.1)10> rclref:list_unique_keys().
{ok,[<<"dog">>]}

(rclref@127.0.0.1)11> rclref:list_all_keys().
{ok,[<<"dog">>,<<"dog">>,<<"dog">>]}

(rclref@127.0.0.1)13> rclref:reap_tombs(<<"dog">>).
ok

(rclref@127.0.0.1)14> rclref:list_all_keys().
{ok,[]}

(rclref@127.0.0.1)15> rclref:list_all_objects().
{ok,[]}
```


The snippet above shows how to use the Low Level API in a `n_val=3`, `r_val=1`, `w_val=1` environment.
It first creates a riak_object with  Key=<<"dog">>, Value=<<"cat">> and then store it to the backend. This actually stores three copies of the object because `n_val=3` but it only confirms one of the stores because `w_val=1`, thus the return value only contain 1 set of the riak_object.
On storing, it will increment the vector clock map. You can see it in the second value of the r_content tuple of the return value.
On get, it will return one set of value because `r_val=1`.
Delete operation is internally convered to a put operation of of a tombstone: Key=<<"dog">>, Value=<<"undefined">>, thus the vector clock is incremented again.
A tombstone in the backend will not be detected by the get operation. Thus a get will return error not_found.
However, coverage requests such as list_all_objects, list_unique_keys, list_all_keys will detect the tombstones in the backend.
Tombstones will be deleted from the backend by reap_tombs. This function should only be used when the connection between nodes are stable.


!!! Note
    The code for the lowlevelapi lies [here](https://github.com/wattlebirdaz/rclref/blob/master/apps/rclref/src/rclref.erl).
