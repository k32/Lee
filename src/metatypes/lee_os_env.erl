%% @doc Read configuration from OS environment variables.
%%
%% This module privides means of mapping OS environment variables to
%% Lee configuration values. Values of environment variables are
%% parsed according to the following rules: Lee values of type
%% `string()' are used verbatim. Values of type `atom()' are
%% transformed using `list_to_atom/1' function and the rest of types
%% are parsed as Erlang terms.
-module(lee_os_env).

-behavior(lee_metatype).

-export([ names/1
        , create/1
        , meta_validate_node/4

        , description_title/2
        , description/2
        , description_node/4

        , read_patch/2
        , read_to/2
        ]).

-include_lib("lee/src/framework/lee_internal.hrl").

-define(prefix_key, [?MODULE, attr_prefix]).
-define(prio_key, [?MODULE, priority]).

-define(metatype, os_env).

create(Conf) ->
    Prefix = maps:get(prefix, Conf, ""),
    Priority = maps:get(priority, Conf, 10),
    [ {?prefix_key, Prefix}
    , {?prio_key, Priority}
    ].

names(_) ->
    [?metatype].

description_title(?metatype, _) ->
    "OS Environment Variables".

description(?metatype, Model) ->
    {ok, Prio} = lee_model:get_meta(?prio_key, Model),
    [{para,
      ["The following OS environment variables are used to
       set configuration values. Values of type string() are
       taken from OS environment variables verbatim, other types
       are parsed as Erlang terms."]},
     {para,
      ["Priority: ", integer_to_list(Prio)]}
     ].

%% @private
meta_validate_node(?metatype, _, Key, MNode) ->
    lee_lib:inject_error_location(
      Key,
      lee_lib:validate_optional_meta_attr( os_env
                                         , typerefl:printable_latin1_list()
                                         , MNode
                                         )).

description_node(os_env, Model, Key, MNode) ->
    #mnode{metaparams = Attrs} = MNode,
    {ok, Prefix} = lee_model:get_meta(?prefix_key, Model),
    EnvVar = Prefix ++ ?m_attr(?metatype, os_env, Attrs, make_default_key(Key)),
    lee_doc:refer_value(Key, ?metatype, EnvVar, MNode).

%% @doc Make a patch from OS environment variables
%% @throws {error, string()}
read_patch(?metatype, Model) ->
    {ok, Prio} = lee_model:get_meta(?prio_key, Model),
    EnvVars = lee_model:get_metatype_index(?metatype, Model),
    {ok, Prio, lists:foldl( fun(Key, Acc) ->
                                    read_val(Model, Key, Acc)
                            end
                          , []
                          , EnvVars)}.

%% @doc Make a patch from OS environment variables and apply it to
%% data
%% @throws {error, string()}
-spec read_to(lee:model(), lee_storage:data()) ->
          lee:patch_result().
read_to(Model, Data) ->
    {ok, _Prio, Patch} = read_patch(?metatype, Model),
    lee:patch(Model, Data, Patch).

%% @private
read_val(Model, Key, Acc) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    {ok, Prefix} = lee_model:get_meta(?prefix_key, Model),
    Default = make_default_key(Key),
    EnvVar = Prefix ++ ?m_attr(?metatype, os_env, Attrs, Default),
    case os:getenv(EnvVar) of
        false ->
            Acc;
        Value0 ->
            case lee:from_string(Model, Key, Value0) of
                {ok, Value} ->
                    [{set, Key, Value} | Acc];
                {error, _} = Error ->
                    throw(Error)
            end
    end.

make_default_key(Key) ->
    lists:flatten(lists:join("__", make_default_key_(Key))).

%-spec make_default_key(lee:key()) -> string().
make_default_key_([]) ->
    [];
make_default_key_([Atom|Rest]) when is_atom(Atom) ->
    [string:to_upper(atom_to_list(Atom))|make_default_key_(Rest)];
make_default_key_([Tuple|_Rest]) when is_tuple(Tuple) ->
    error(sorry_not_supported).
