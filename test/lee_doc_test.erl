-module(lee_doc_test).

-behavior(lee_metatype).

-export([names/1, create/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

-include("lee.hrl").

long_text() ->
    lee_doc:from_file(lee, "loremipsum.xml").

model() ->
    Model = #{ '$doc_root' =>
                   {[doc_root],
                    #{ oneliner => "This is a test model for doc extraction"
                     , app_name => "Lee Test Application"
                     , doc => long_text()
                     , prog_name => "lee_test"
                     }}
             , foo =>
                   {[value, os_env],
                    #{ oneliner => "This parameter controls fooing"
                     , type     => typerefl:string()
                     , default  => "foo"
                     , doc      => long_text()
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
             , cli => lee_cli_tests:test_model_raw()
             },
    {ok, Mod} = lee_model:compile( [ lee:base_metamodel()
                                   , lee_metatype:create(lee_os_env, #{prefix => "TEST_"})
                                   , lee_metatype:create(lee_config_file, #{ tag => conf_file
                                                                           , file => "/etc/myapp.eterm"
                                                                           })
                                   %% , lee_consult:metamodel()
                                   , lee_metatype:create(lee_cli)
                                   , lee_metatype:create(?MODULE)
                                   ]
                                 , [Model]
                                 ),
    Mod.


export_test() ->
    MTs = [ cli_param
          , os_env
          , conf_file
          %% , {consult, #{ filter      => [foo]
          %%              , config_name => "foo.conf"
          %%              }}
          %% , {consult, #{ filter      => [bar]
          %%              , config_name => "bar.conf"
          %%              }}
          , value
          ],
    Config = #{ metatypes => MTs
              , run_pandoc => false
              , doc_xml => "test/data/external_doc.xml"
              },
    lee_doc:make_docs(model(), Config).

%% Metatype callbacks:

create(_) ->
    [].

names(_) ->
    [foo, bar].
