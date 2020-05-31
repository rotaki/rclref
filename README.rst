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

Licensed under the Apache License, Version 2.0 (the “License”);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
