rclref
===========

A reference implementation of a distributed key-value store using riak_core_lite.
More information on this project is provided `here <https://wattlebirdaz.github.io/rclref>``_.

Build
-----

::

    make release

Test
----

::

    make ct

Run
---

::

    make console

Try
---

::

    1> rclref_client:put(<<"dog">>, <<"cat">>).
    ok

    2> rclref_client:get(<<"dog">>).
    {ok,[<<"cat">>,<<"cat">>,<<"cat">>]}

    3> rclref_client:list_keys().
    {ok,[<<"dog">>]}

    4>rclref_client:delete(<<"dog">>).
    ok

    5> rclref_client:get(<<"dog">>).
    {error,not_found}



More information:

* `rclref development site <https://wattlebirdaz.github.io/rclref>``_
* `Getting Started Guide <https://riak-core-lite.github.io/blog/pages/getting-started/>`_
* `Riak Core Lite Site <https://riak-core-lite.github.io/>``_

License
-------

Copyright 2020 Riki Otaki

Licensed under the Apache License, Version 2.0
