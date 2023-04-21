-module(lee).

%% API exports
-export([ namespace/2
        , base_metamodel/0
        , get/2, get/3, list/2, list/3
        , validate/2
        , meta_validate/1
        , from_string/3, from_strings/3

        , patch/2, patch/3
        , init_config/2
        ]).

-export_type([node_id/0, metatype/0, type/0, mnode/0, model/0,
              lee_module/0, cooked_module/0, properties/0, data/0, patch/0, patch_result/0
             ]).

-include_lib("typerefl/include/types.hrl").
-include("lee_internal.hrl").

%%====================================================================
%% Types
%%====================================================================

-type data() :: lee_storage:data() | [lee_storage:data()].

-type patch() :: lee_storage:patch(term()).

-type namespace() :: #{node_id() => mnode() | namespace()}.

-type lee_module() :: namespace().

-type cooked_module() :: lee_storage:data(#mnode{}).

-type model() :: #model{}.

-type validate_result() :: {ok, Warnings :: [string()]}
                         | {error, Errors :: [term()], Warnings :: [term()]}.

-type patch_result() :: {ok, data(), Warnings :: [string()]}
                      | {error, Errors :: [term()], Warnings :: [term()]}.

-type metatype() :: atom().

-type node_id() :: atom() | {}.

-type model_key() :: [node_id()].

-type key() :: [atom() | tuple()].

-type properties() :: #{atom() => term()}.

-type type() :: #type{} | atom() | {var, term()}.

%% Model node
-type mnode() :: {[metatype()], properties(), lee_module()}
               | {[metatype()], properties()}. %% Shortcut for child-free MOs

-reflect_type([key/0, model_key/0]).

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% API functions
%%====================================================================

%% @doc Put model to a namespace.
%%
%% `namespace([foo, bar], A)' is equivalent to `#{foo => #{bar => A}}'
-spec namespace(lee:key(), lee:module()) ->
                       lee:module().
namespace(Key, M) ->
    lists:foldl( fun(NodeId, Acc) ->
                         #{NodeId => Acc}
                 end
               , M
               , lists:reverse(Key)
               ).

%% @doc Model module containing basic metatypes:
-spec base_metamodel() -> [lee_metatype:cooked_metatype()].
base_metamodel() ->
    [ lee_metatype:create(lee_value)
    , lee_metatype:create(lee_map)
    , lee_metatype:create(lee_doc_root)
    , lee_metatype:create(lee_undocumented)
    , lee_metatype:create(lee_pointer)
    , lee_metatype:create(lee_app_env)
    ].

-spec get(data(), lee:key()) -> term().
get(Data, Key) ->
    get(get_bakedin_model(Data), Data, Key).

%% @doc Get a value from the config
%%
%% The return value is guaranteed safe, as long as `Data' has been
%% validated against the `Model', and `Key' is a valid `Model' key
-spec get(model() | lee:cooked_module(), data(), lee:key()) -> term().
get(Model, Data, Key) when ?is_storage(Data) ->
    case lee_storage:get(Key, Data) of
        {ok, Val} ->
            Val;
        _ ->
            MKey = lee_model:get_model_key(Key),
            #mnode{metaparams = Attrs} = lee_model:get(MKey, Model),
            case Attrs of
                #{default := Val} ->
                    Val;
                #{default_ref := RefKey} ->
                    get(Model, Data, RefKey);
                _ ->
                    %% Data shouldn't have passed validation without
                    %% the default, so just crash here
                    error({missing_data, Key})
            end
    end;
get(Model, [Data], Key) ->
    get(Model, Data, Key);
get(Model, [Data|Rest], Key) ->
    case lee_storage:get(Key, Data) of
        {ok, Val} ->
            Val;
        undefined ->
            get(Model, Rest, Key)
    end.

-spec init_config(model(), data()) -> patch_result().
init_config(Model, Data0) ->
    Patches0 = [lee_metatype:read_patch(MT, Model)
                || MT <- lee_model:all_metatypes(Model)],
    Errors = [Err || {error, Errors} <- Patches0, Err <- Errors],
    case Errors of
        [] ->
            Patches = lists:keysort(2, Patches0),
            Patch = lists:flatmap(fun({ok, _Prio, Val}) -> Val end, Patches),
            case lee:patch(Model, Data0, Patch) of
                Ret = {ok, Data, _} ->
                    post_init(Model, Data, Patch),
                    Ret;
                Err ->
                    Err
            end;
        _ ->
            {error, Errors, []}
    end.

-spec patch(data(), patch()) -> patch_result().
patch(Data, Patch) ->
    patch(get_bakedin_model(Data), Data, Patch).

-spec patch(model(), data(), patch()) -> patch_result().
patch(Model, Data0, Patch) ->
    %% TODO: Inefficient. Make an overlay storage
    PendingData0 = lee_storage:clone(Data0, lee_map_storage, #{}),
    PendingData = lee_storage:patch(PendingData0, Patch),
    case lee:validate(Model, PendingData) of
        {ok, Warnings} ->
            Data = lee_storage:patch(Data0, [{set, ?bakedin_model_key, Model} | Patch]),
            lists:foreach(fun(PatchOp) ->
                                  process_patch_op(Model, Data, PatchOp)
                          end,
                          Patch),
            {ok, Data, Warnings};
        Err ->
            Err
    end.

-spec list(data(), lee:key()) -> [lee:key()].
list(Data, Key) ->
    list(get_bakedin_model(Data), Data, Key).

%% @doc List objects in `Data' that can match `Key'
%%
%% `?children' nodes in the key will be replaced with actual child
%% keys, e.g. ```list(Model, Data, [foo, ?children])''' will return
%% ```[[foo, {1}], [foo, {2}]]''' when map `[foo]' contains
%% two children with keys `1' and `2'.
-spec list(model() | cooked_module(), data(), lee:key()) -> [lee:key()].
list(_Model, Data, Key) when ?is_storage(Data) ->
    %% TODO: get metatype index of value metatype and add it here as default?
    lee_storage:list(Key, Data);
list(Model, Data, Pattern) when is_list(Data) ->
    lists:usort(lists:foldl( fun(I, Acc) ->
                                     list(Model, I, Pattern) ++ Acc
                             end
                           , []
                           , Data)).

%% @doc Validate `Data' against `Model'.
-spec validate(lee:model(), data()) -> validate_result().
validate(Model, Data) ->
    PerNodeCallback =
        fun(Metatype, Model1, MKey, MNode) ->
                do_validate_data(Metatype, Model1, Data, MKey, MNode)
        end,
    PerMtCallback =
        fun(Metatype, {ErrAcc, WarnAcc}) ->
                {Err, Warn} = lee_metatype:validate(Metatype, Model, Data),
                {Err ++ ErrAcc, Warn ++ WarnAcc}
        end,
    case validate_nodes(PerNodeCallback, Model) of
        {[], Warn0} ->
            case lee_model:fold_metatypes(PerMtCallback, {[], []}, Model) of
                {[], Warn1} ->
                    {ok, Warn1 ++ Warn0};
                {Err, Warn1} ->
                    {error, Err, Warn1 ++ Warn0}
            end;
        {Err, Warn} ->
            {error, Err, Warn}
    end.

%% @doc Validate the `Model' itself against its metamodel. Useful for
%% spotting bugs in the model definition
-spec meta_validate(lee:model()) -> lee_metatype:metavalidate_result().
meta_validate(Model) ->
    PerNodeCallback = fun meta_validate_node/4,
    PerMtCallback =
        fun(Metatype, {ErrAcc, WarnAcc, PatchAcc}) ->
                {Err, Warn, Patch} = lee_metatype:meta_validate(Metatype, Model),
                {Err ++ ErrAcc, Warn ++ WarnAcc, Patch ++ PatchAcc}
        end,
    case validate_nodes(PerNodeCallback, Model) of
        {[], Warn0} ->
            {Err, Warn1, Patch} =
                lee_model:fold_metatypes(PerMtCallback, {[], [], []}, Model),
            {Err, Warn1 ++ Warn0, Patch};
        {Err, Warn} ->
            {Err, Warn, []}
    end.

-spec from_string(lee:model(), lee:model_key(), string()) ->
                         {ok, term()} | {error, string()}.
from_string(Model, Key, String) ->
    #mnode{metaparams = MP} = lee_model:get(Key, Model),
    Type = maps:get(type, MP),
    Default = fun(Str) ->
                      case typerefl:from_string(Type, Str) of
                          Ok = {ok, _} ->
                              Ok;
                          {error, Err} ->
                              {error, Err}
                      end
              end,
    Fun = maps:get(from_string, MP, Default),
    Fun(String).

%% @doc Temporary function, don't use it.
-spec from_strings(lee:model(), lee:model_key(), [string()]) ->
                          {ok, [term()]} | {error, string()}.
from_strings(Model, Key, Strings) ->
    #mnode{metaparams = MP} = lee_model:get(Key, Model),
    Type = ?m_attr(value, type, MP),
    try
        case MP of
            #{?m_valid(value, from_string) := Fun} ->
                %% Custom from_string callback is defined:
                {ok, [case Fun(I) of
                          {ok, T}          -> T;
                          Err = {error, _} -> throw(Err)
                      end || I <- Strings]};
            _ ->
                %% Fallback to typerefl:
                {ok, typerefl:from_string_(Type, Strings)}
        end
    catch
        Err = {error, _} -> Err
    end.

%%====================================================================
%% Internal functions
%%====================================================================

process_patch_op(Model, Data, PatchOp) ->
    case PatchOp of
        {set, Key, _} -> ok;
        {rm, Key} -> ok
    end,
    MNode = lee_model:get(lee_model:get_model_key(Key), Model),
    #mnode{metatypes = MTs} = MNode,
    lists:foreach(fun(MT) ->
                          lee_metatype:post_patch(MT, Model, Data, MNode, PatchOp)
                  end,
                  MTs).

post_init(Model, Data, Patch) ->
    ModelIdx = Model#model.meta_class_idx,
    PatchKeys = lists:map(fun lee_lib:patch_key/1, Patch),
    Keys0 = maps:fold(
              fun(MT, Keys, Acc) ->
                      case lee_metatype:is_implemented(Model, MT, post_patch) of
                          true ->
                              Keys ++ Acc;
                          false ->
                              Acc
                      end
              end,
              [],
              ModelIdx),
    Keys1 = lists:usort(Keys0) -- PatchKeys,
    Keys = lists:filter(fun(K) -> not lists:member(?children, K) end, Keys1),
    lists:foreach(
      fun(Key) ->
              try %% TODO: Ugly
                  Val = get(Model, Data, Key),
                  process_patch_op(Model, Data, {set, Key, Val})
              catch
                  _:_ -> ok
              end
      end,
      Keys).

%% Validate all instances of metatypes against the model
-spec validate_nodes( fun((metatype(), model(), model_key(), #mnode{}) -> lee_lib:check_result())
                       , lee:model()
                       ) -> lee_lib:check_result().
validate_nodes(Fun, Model) ->
    lee_model:fold_mt_instances(
      fun(MT, Instance, {ErrAcc, WarnAcc}) ->
              MNode = lee_model:get(Instance, Model),
              {Err, Warn} = Fun(MT, Model, Instance, MNode),
              {Err ++ ErrAcc, Warn ++ WarnAcc}
      end,
      {[], []},
      Model).

-spec do_validate_data(metatype(), model(), data(), model_key(), #mnode{}) ->
          lee_lib:check_result().
do_validate_data(Metatype, Model, Data, MKey, MNode) ->
    Instances = case lee_model:split_key(MKey) of
                    {[], _} ->
                        [MKey];
                    {Optional, Required} ->
                        [ParentMapKey ++ Required || ParentMapKey <- lee_storage:list(Optional, Data)]
                end,
    lists:foldl( fun(Key, {ErrAcc, WarnAcc}) ->
                         {Err, Warn} = lee_lib:inject_error_location(Key, lee_metatype:validate_node(Metatype, Model, Data, Key, MNode)),
                         {Err ++ ErrAcc, Warn ++ WarnAcc}
                 end
               , {[], []}
               , Instances
               ).

-spec meta_validate_node(metatype(), model(), key(), #mnode{}) -> lee_lib:check_result().
meta_validate_node(MT, Model, Key, MNode = #mnode{metaparams = MP}) ->
    MPSpec = lee_metatype:metaparams(MT, Model),
    Type = typerefl:map([{case Kind of
                              mandatory -> strict;
                              _         -> fuzzy
                          end, K, Val} || {Kind, K, Val} <- MPSpec] ++ [{fuzzy, term(), term()}]),
    Warnings = [lee_lib:format("Missing optional ~p metaparameter", [SpecKey])
                || {warn_if_missing, SpecKey, _} <- MPSpec
                 , not maps:is_key(SpecKey, MP)],
    {Errors, Warn} = case typerefl:typecheck(Type, MP) of
                         ok           -> lee_metatype:meta_validate_node(MT, Model, Key, MNode);
                         {error, Err} -> {[lee_lib:format("Metaparameters of ~p are invalid. ~s",
                                                          [MT, lee_lib:format_typerefl_error(Err)])], []}
                     end,
    lee_lib:inject_error_location(Key, {Errors, Warnings ++ Warn}).

-spec get_bakedin_model(data()) -> model().
get_bakedin_model(Data) ->
    case lee_storage:get(?bakedin_model_key, Data) of
        {ok, Model} -> Model;
        undefined   -> error("Data has not been initilized properly")
    end.
