BASEDIR = $(shell pwd)
REBAR = ./rebar3
RELPATH = _build/default/rel/rclref
PRODRELPATH = _build/prod/rel/rclref
DEV1RELPATH = _build/dev1/rel/rclref
DEV2RELPATH = _build/dev2/rel/rclref
DEV3RELPATH = _build/dev3/rel/rclref
APPNAME = rclref
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../rclref_config

console:
	cd $(RELPATH) && ./bin/rclref console

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) daemon

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) daemon_attach

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../rclref_config

prod-console:
	cd $(PRODRELPATH) && ./bin/rclref console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer

format:
	$(REBAR) format

ct: 
	$(REBAR) ct --sname test

devrel: devrel1 devrel2 devrel3

devrel-clean:
	rm -rf _build/dev*/rel

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rclref/bin/$(APPNAME) daemon; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/rclref/bin/$(APPNAME) eval 'riak_core:join("rclref1@127.0.0.1")'; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) eval 'riak_core_claimant:plan()'

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) eval 'riak_core_claimant:commit()'

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rclref/bin/$(APPNAME) ping; true; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rclref/bin/$(APPNAME) stop; true; done

devrel1:
	$(REBAR) as dev1 release
	mkdir -p $(DEV1RELPATH)/../rclref_config

devrel2:
	$(REBAR) as dev2 release
	mkdir -p $(DEV2RELPATH)/../rclref_config

devrel3:
	$(REBAR) as dev3 release
	mkdir -p $(DEV3RELPATH)/../rclref_config

dev1-start:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) daemon

dev2-start:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) daemon

dev3-start:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) daemon

dev1-stop:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) stop

dev2-stop:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) stop

dev3-stop:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) stop

dev1-attach:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) daemon_attach

dev2-attach:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) daemon_attach

dev3-attach:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) daemon_attach

dev1-console:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) console

dev2-join:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) eval 'riak_core:join("rclref1@127.0.0.1")'

dev3-join:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) eval 'riak_core:join("rclref1@127.0.0.1")'

dev1-leave:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) eval 'riak_core:leave()'

dev2-leave:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) eval 'riak_core:leave()'

dev3-leave:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) eval 'riak_core:leave()'

dev1-status:
	$(BASEDIR)/_build/dev1/rel/rclref/bin/$(APPNAME) eval 'riak_core_console:member_status([])'

dev2-status:
	$(BASEDIR)/_build/dev2/rel/rclref/bin/$(APPNAME) eval 'riak_core_console:member_status([])'

dev3-status:
	$(BASEDIR)/_build/dev3/rel/rclref/bin/$(APPNAME) eval 'riak_core_console:member_status([])'


