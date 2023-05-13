-module(lee_doc_test).

-behavior(lee_metatype).

-export([names/1, create/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

-include("lee.hrl").

model() ->
    Model = #{ '$doc_root' =>
                   {[doc_root],
                    #{ oneliner => "This is a test model for doc extraction"
                     , app_name => "Lee Test Application"
                     , prog_name => "lee_test"
                     }}
             , foo =>
                   {[value, os_env],
                    #{ oneliner => "This parameter controls fooing"
                     , type     => typerefl:string()
                     , default  => "foo"
                     }}
             , bar =>
                   {[value, os_env],
                    #{ oneliner => "This parameter controls baring"
                     , type     => {typerefl:nonempty_list([]), typerefl:iolist()}
                     , os_env   => "BAR"
                     }}
             , baz =>
                   {[value],
                    #{ type => {typerefl:nonempty_list([]), typerefl:iolist()}
                     }}
             , quux =>
                   {[value, foo],
                    #{ type     => integer()
                     , oneliner => "This value controls quuxing"
                     }}
             , xizzy =>
                   {[value, bar],
                    #{ type     => float()
                     , oneliner => "This value controls xizzying"
                     }}
             , more_stuff =>
                   #{ default_ref =>
                          {[value, os_env],
                           #{ type        => typerefl:string()
                            , default_ref => [foo]
                            }}
                    , default_str =>
                          {[value],
                           #{ type => typerefl:ip_address()
                            , default_str => "127.0.0.1"
                            }}
                    }
             , cli => lee_cli_tests:test_model_raw()
             },
    Options = #{readme_file => "doc/src/README.adoc"},
    {ok, Mod} = lee_model:compile( [ lee_metatype:create(lee_os_env, #{prefix => "TEST_"})
                                   , lee_metatype:create(lee_config_file, #{ tag => conf_file
                                                                           , file => "/etc/myapp.eterm"
                                                                           })
                                   %% , lee_consult:metamodel()
                                   , lee_metatype:create(lee_cli)
                                   , lee_metatype:create(?MODULE)
                                   | lee:base_metamodel()
                                   ]
                                 , [lee_asciidoc:enrich_model(Options, Model)]
                                 ),
    Mod.

doc_root_test() ->
    Model = model(),
    ?assertMatch( [ {title, ["Lee Test Application"]}
                  , {preface,
                     [ {title, ["Introduction"]}
                     | _
                     ]}
                  ]
                , lee_metatype:description(doc_root, Model, #{})
                ).

export_test() ->
    OutFile = "_build/lee_doc/test_out.xml",
    Config = #{ output_file => OutFile
              },
    ok = lee_doc:make_docs(model(), Config).

%% Metatype callbacks:

create(_) ->
    [].

names(_) ->
    [foo, bar].
