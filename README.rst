rclref
===========

A riak_core application

Build
-----

::

    rebar3 release

Test
----

::

    rebar3 ct

Run
---

::

    ./_build/default/rel/rclref/bin/rclref console

Try
---

::

    1> rclref:ping().
    {pong,'rclref1@127.0.0.1', 9...8}

Quit
----

::

    2> q().

More information:

* `Getting Started Guide <https://riak-core-lite.github.io/blog/pages/getting-started/>`_
* `Riak Core Lite Site <https://riak-core-lite.github.io/>`_


License
-------

Copyright 2020 Riki Otaki

Licensed under the Apache License, Version 2.0
