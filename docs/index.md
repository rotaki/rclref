# Welcome to the rclref develoment site

riak_core is a distributed systems framework that is based on the Dynamo-Style architecture. It has served as the basis of Riak KV, a highly available masterless distributed database, providing essential features for controlling the behaviour of nodes in a database cluster. riak_core_lite is an updated version of riak_core which ensures compatibility with the recent OTP version and rebar, a build tool for erlang application and releases. 


**rclref** is a reference implementation of a key-value store built on riak_core_lite. rclref was created as part of Google Summer of Code 2020 with the aim of exercising various APIs of riak_core_lite with documented support. This implementation can also be used for detecting regressions and serve as a starting point to try new ideas and show how different parts of riak_core_lite are used in practice.

!!! Tip
Updated version of the code is [here](https://github.com/wattlebirdaz/rclref).

# Summary


## Describe my work briefly

- Implemented a riak_core_lite based distributed application.

## What is done

- Created a RiakKV-like sample key-value store application on riak_core_lite with documents of the implemented features.
- Introduced how to test an Erlang distributed application using Common Test framework.
- Introduced how to benchmark key-value application using `rcl_bench`.

## TODO

- Show benchmark results of rclref with riak_core backend and compare it with riak_core_lite.
- Abstract the testing module for more generic use.
- Use CRDTs for merging the replicated values.

# Development languages and tools
The code is fully written in Erlang OTP >= 22.

# License
rclref is released under Apache License, Version 2.0

# Table of Cotents

1. [How to setup riak_core_lite application?](setup.md)
2. [How are put, get, delete implemented in rclref?](put_get_delete.md)
3. [How to test distributed Erlang application using Common Test?](test.md)
4. [How to benchmark riak_core_lite application?](benchmark.md)
5. [How to actually use rclref?](usage.md)
6. [Other](other.md)
