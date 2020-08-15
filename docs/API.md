
# rclref_client

## Module 

rclref_client

## Module Summary

Basic functions for manipulating rclref

## Description

This module provides basic funcitons for reading data from and storing data into rclref, a key-value store on riak-core-lite.


1.  [put(Key, Value)](#org2a20286)
2.  [put(Key, Value, Options)](#org6c12821)
3.  [get(Key)](#org382c11b)
4.  [get(Key, Options)](#orga550d0d)
5.  [delete(Key)](#org0e2e4db)
6.  [delete(Key, Options)](#orgc8da9eb)
7.  [reap_tombs(Key)](#orge119355)
8.  [reap_tombs(Key, Options)](#orgad0f234)


<a id="org2a20286"></a>

# put(Key, Value)

    -spec put(rclref_object:key(), rclref_object:value()) -> ok | {error, timeout} | {error, partial} |  {error, term()}.

This is equal to `rclref_put(Key, Value, [])`


<a id="org6c12821"></a>

# put(Key, Value, Options)

    -spec put(rclref_object:key(), rclref_object:value(), [term()]) -> ok | {error, timeout} | {error, partial} | {error, term()}.

store a Key-Value in N vnodes. When W vnodes respond with ok or more than N-W number of vnodes respond with an error, this function will return. If neither of these is satisified within TIMEOUT_PUT, then this function will return {error, timeout}

  - Returns `ok` when W vnodes respond with ok.
  - Returns `{error, partial}` when at least one of the vnodes (but not more than or equal to W vnodes) responds with ok before getting errors from more than N-W vnodes.
  - Returns `{error, [Reason]}` when no vnodes respond with ok and more than N-W vnodes respond with an error.  `[Reason]` is a list that contains error reasons of N-W+1 vnodes.
  - Returns `{error, timeout}` when neither of these above are satisfied within TIMEOUT_PUT milliseconds.


<a id="org382c11b"></a>

# get(Key)

    -spec get(rclref_object:key()) -> {ok, [rclref_object:value()]} | {error, timeout} | {error, partial} | {error, not_found} | {error, term()}.


This is equal to `get(Key, Value, Options)`.

<a id="orga550d0d"></a>

# get(Key, Options)

    -spec get(rclref_object:key(), [term()]) -> {ok, [rclref_object:value()]} | {error, timeout} | {error, partial} | {error, not_found} | {error, term()}.

get a Key-Value from N vnodes. When R number of vnodes respond with ok or more than N-R number of vnodes respond with an error, this fucntion will return. If neither of these is satisified with TIMEOUT_GET, then this function will return {error, timeout}

On get, response from a vnode will be the either of `{ok, RObj}`,`{error, not_found}` , `{error, Reason}`  

<a id="orgefd1cd5"></a>


- Returns `{ok, [Value]}` when R vnodes respond with a value.   `[Value]` is a list that contains values from R vnodes.
- Returns `{error, partial}` when at least one of the vnodes (but not more than or equal to R vnodes) responds with a value before getting errors from more than N-R vnodes.
- Returns `{error, not_found}` when no vnodes respond with a value and more than N-R vnodes respond with a error which are all not_found.
- Returns `{error, [Reason]}` when no vnodes respond with a value and more than N-R vnodes respond with a error which are not all not_found. `[Reason]` is a list that contains error reasons of N-R+1 vnodes.
- Returns `{error, timeout}` when neither of these above is satisified within TIMEOUT_GET milliseconds.

<a id="org0e2e4db"></a>

# delete(Key)

    -spec delete(rclref_object:key()) -> ok | {error, timeout} | {error, partial} | {error, term()}.

This is equal to `delete(Key, [])`.


<a id="orgc8da9eb"></a>

# delete(Key, Options)

    -spec delete(rclref_object:key(), [term()]) -> ok | {error, timeout} | {error, partial} | {error, term()}.

This is equal to `put(Key, undefined, Options)`.
Note that this will not delete the Key from the backend, however `rclref_client:get(Key)` will return `{error, not_found}` after calling this function.
The reason why this is implemeted this way is explained in the [TODO] section.
A Key-Value with an undefined value is called a tombstone.


<a id="orge119355"></a>

# list_keys()

    -spec list_keys() -> {ok, [rclref_object:key()]}. 

This is equal to `list_keys([])`.

<a id="orgad0f234"></a>

# list_keys(Options)

    -spec list_keys([term()]) -> {ok, [rclref_object:key()]}. 


list all unique keys in the backend.

