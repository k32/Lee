-module(lee_os_env_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

model() ->
    Model0 = #{ home => {[value, os_env],
                         #{ os_env => "HOME"
                          , type => string()
                          }}
              , path => {[value, os_env, app_env],
                         #{ type => string()
                          , app_env => {lee, path}
                          , app_env_transform => fun list_to_binary/1
                          }}
              },
    Meta = [lee:base_metamodel(), lee_metatype:create(lee_os_env),
            lee_metatype:create(lee_app_env)],
    {ok, Model} = lee_model:compile(Meta, [Model0]),
    Model.

osenv_test() ->
    ok = application:unset_env(lee, path),
    Model = model(),
    Data = lee:init_config(Model, lee_storage:new(lee_map_storage)),
    Home = os:getenv("HOME"),
    Path = os:getenv("PATH"),
    ?assertMatch( Home
                , lee:get(Model, Data, [home])
                ),
    ?assertMatch( Path
                , lee:get(Model, Data, [path])
                ),
    ?assertEqual( {ok, list_to_binary(Path)}
                , application:get_env(lee, path)
                ),
    _ = lee:patch(Model, Data, [{rm, [path]}]),
    ?assertMatch( undefined
                , application:get_env(lee, path)
                ).
