# How to a setup riak_core_lite application?

This page provides an overview of how to setup a riak_core_lite applicaiton.

## Erlang

Install Erlang OTP version >= 22

## rebar3

Install rebar3

## riak_core_lite

Clone riak_core_lite template by the following command:
```sh
mkdir -p ~/.config/rebar3/templates
git clone https://github.com/riak-core-lite/rebar3_template_riak_core_lite.git ~/.config/rebar3/templates/rebar3_template_riak_core_lite
```
Create a new riak_core_lite project:
```sh
rebar3 new rebar3_riak_core_lite name=YOURAPP
```

See [here](https://riak-core-lite.github.io/blog/pages/getting-started/) for details.

## elvis

Elvis is a Erlang style reviewer. 
Install Elvis from [here](https://github.com/inaka/elvis).
Configure it by creating `elvis.config` in the repository. The following snippet is the configuration used in rclref.

```erlang
%linting and style rules
[{elvis,
    [{config,
        [#{dirs => ["apps/*/src"],
            filter => "*.erl",
            rules => [{elvis_style, line_length,
                        #{ignore => [],
                            limit => 100,
                            skip_comments => false}},
                        {elvis_style, no_tabs},
                        {elvis_style, no_trailing_whitespace},
                        {elvis_style, macro_names, #{ignore => []}},
                        {elvis_style, macro_module_names},
                        {elvis_style, operator_spaces, #{rules => [{right, ","},
                                                                    {right, "++"},
                                                                    {left, "++"},
                                                                    {right, "--"},
                                                                    {left, "--"}]}},
                        %{elvis_style, god_modules,
                        %#{limit => 40,  
                        %    ignore => []}},
                        {elvis_style, used_ignored_variable},
                        {elvis_style, no_behavior_info},
                        {
                            elvis_style,
                            module_naming_convention,
                            #{regex => "^[a-z]([a-z0-9]*_?)*(_SUITE)?$", 
                                ignore => []}
                        },
                        {
                           elvis_style,
                           function_naming_convention,
                           #{regex => "^[a-z]([a-z0-9]*_?)*$"} %base: ^([a-z][a-z0-9]*_?)*$
                        },
                        {elvis_style, state_record_and_type},
                        {elvis_style, no_spec_with_records}
                        ]
            },
            #{dirs => ["."],
            filter => "Makefile",
            rules => [{elvis_project, no_deps_master_erlang_mk, #{ignore => []}},
                        {elvis_project, protocol_for_deps_erlang_mk, #{ignore => []}}]
            },
            #{dirs => ["."],
            filter => "rebar.config",
            rules => [{elvis_project, no_deps_master_rebar, #{ignore => []}},
                        {elvis_project, protocol_for_deps_rebar, #{ignore => []}}]
            }
        ]
    }]
}].
```

After configuring, the code can be reviewed by:

```sh
elvis rock --config elvis.config
```

## rebar3_format

rebar3_format is a code formatter for Erlang.

Add the following lines to `rebar.config`.

```erlang
{plugins, [rebar3_format]}.
{format, [{files, ["apps/rclref/src/*.erl", "test/*.erl", "test/utils/*.erl"]}]}.
```

Then the code can be formatted by

```sh
rebar3 format
```

## dialyzer

A static type checking can be done by:
```sh
rebar3 dialyzer
```
