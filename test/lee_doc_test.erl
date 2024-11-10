-module(lee_doc_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

-include("lee.hrl").

model() ->
    Model = #{ cli_arg1 =>
                   {[value, cli_param],
                    #{ oneliner => "CLI parameter at top level"
                     , type => string()
                     , default => "hello"
                     , cli_operand => "cli-arg1"
                     , cli_short => $a
                     }}
             , cli_positional =>
                   {[value, cli_positional],
                    #{ oneliner => "Positional argument 1"
                     , doc =>
"
This is documentation for positional argument 1

It goes on and on.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua.  Ut enim
ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
aliquip ex ea commodo consequat.  Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur.  Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
"
                     , type => integer()
                     , cli_arg_position => 1
                     }}
             , cli_positional2 =>
                   {[value, cli_positional],
                    #{ oneliner => "Positional argument 2"
                     , type => integer()
                     , cli_arg_position => 10
                     }}
             , cli_positional_rest =>
                   {[value, cli_positional],
                    #{ oneliner => "Positional argument rest"
                     , type => [integer()]
                     , cli_arg_position => rest
                     }}
             , foo =>
                   {[map, cli_action],
                    #{ cli_operand => "foo"
                     , key_elements => [[foo_long]]
                     , oneliner => "This is a oneliner for foo"
                     , doc =>
"
This is a long description of a CLI action

It goes on and on
"
                     },
                    #{ foo_long =>
                           {[value, cli_param],
                            #{ oneliner => "This is a oneliner for @code{--foo-long}"
                             , doc =>
"
This is a long description of --foo-long.

It goes on and on.
"
                             , type => atom()
                             , default => some_default_value
                             , cli_operand => "foo-long"
                             }}
                     }}
             },
    {ok, Mod} = lee_model:compile( [ lee_metatype:create(lee_os_env, #{prefix => "TEST_"})
                                   , lee_metatype:create(lee_cli)
                                   | lee:base_metamodel()
                                   ]
                                 , [Model]
                                 ),
    Mod.

export_test() ->
    OutDir = "_build/lee_doc/",
    Config = #{ output_dir => OutDir
              , extension => ".texi"
              , formatter => fun lee_doc:texinfo/3
              , metatypes => [cli_param, value]
              },
    ?assertEqual(
       [OutDir ++ "cli_param.texi", OutDir ++ "value.texi"],
       lee_doc:make_docs(model(), Config)).
