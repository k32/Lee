-module(lee).

%% API exports
-export([ namespace/2
        , base_model/0
        , base_metamodel/0
        , metametamodel/0
        , validate_term/3
        , get/3
        , list/3
        , validate/2
        , validate/3
        , from_string/3
        ]).

-export_type([ node_id/0
             , model_key/0
             , key/0
             , metatype/0
             , type/0
             , mnode/0
             , model/0
             , lee_module/0
             , cooked_module/0
             , properties/0
             , data/0
             , patch/0
             ]).

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

-type validate_callback() :: fun((model(), data(), key(), #mnode{}) ->
                                        validate_result()).

-type metatype() :: atom().

-type node_id() :: atom() %% Should not begin with `$', these are reserved
                 | tuple()
                 | integer()
                 .

-type model_key() :: [ node_id() | ?children ].

-type key() :: [ node_id() | ?lcl(term()) | ?children ].

-type properties() :: #{atom() => term()}.

-type type() :: #type{} | atom() | {var, term()}.

%% Model node
-type mnode() :: {[metatype()], properties(), module()}
               | {[metatype()], properties()} %% Shortcut for child-free MOs
               .

%%====================================================================
%% Macros
%%====================================================================

-define(typedef(Name, Arity, Validate, Rest),
        {Name, Arity} => {[type]
                         , #{ validate => fun lee_types:Validate/3
                            , typename => ??Name
                            } Rest
                         }).

-define(typedef(Name, Arity, Validate),
        ?typedef(Name, Arity, Validate, #{})).

-define(typedef(Name, Validate),
        ?typedef(Name, 0, Validate)).

-define(print(Fun), #{print => fun lee_types:Fun/2}).

-define(read(Fun), #{from_string => fun lee_types:Fun/3}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Put model to a namespace
%%
%% ```namespace([foo, bar], A)''' is equivalent to writing
%% ```#{foo => #{bar => A}}'''
-spec namespace(lee:key(), lee:module()) ->
                       lee:module().
namespace(Key, M) ->
    lists:foldl( fun(NodeId, Acc) ->
                         #{NodeId => Acc}
                 end
               , M
               , lists:reverse(Key)
               ).

%% @doc Model fragment containing base types.
-spec base_model() -> lee:module().
base_model() ->
    namespace([lee, base_types]
             , #{ ?typedef(union,      2, validate_union,     ?print(print_union)     )
                , ?typedef(term,          validate_term                               )
                , ?typedef(integer,    0, validate_integer,   ?print(print_integer)   )
                , ?typedef(float,         validate_float                              )
                , ?typedef(atom,       0, validate_atom,      ?read(atom_from_string) )
                , ?typedef(binary,        validate_binary                             )
                , ?typedef(tuple,         validate_any_tuple                          )
                , ?typedef(tuple,      1, validate_tuple,     ?print(print_tuple)     )
                , ?typedef(list,       1, validate_list,      ?print(print_list)      )
                , ?typedef(map,        2, validate_map                                )
                , ?typedef(exact_map,  1, validate_exact_map, ?print(print_exact_map) )
                }
             ).

%% @doc Model fragment containing basic configuration validation
%% metaclasses
-spec base_metamodel() -> lee:module().
base_metamodel() ->
    Model = namespace([metatype]
                     , #{ value =>
                              {[metatype]
                              , #{validate_node => fun validate_value/4}
                              }
                        , map =>
                              {[metatype]
                              , #{}
                              }
                        , type => {[metatype] , #{}}
                        , typedef => {[metatype] , #{}}
                        }
                     ),
    {ok, Val} = lee_model:merge([Model, base_model()]),
    Val.

%% @doc A model validating metamodels
-spec metametamodel() -> lee:module().
metametamodel() ->
    MetaModel = #{
                 },
    {ok, Result} = lee_model:merge([MetaModel, base_model()]),
    Result.

-spec validate_term( lee:model()
                   , lee:type()
                   , term()
                   ) -> lee_types:validate_result().
validate_term(_Model, Atom, Term) when is_atom(Atom) ->
    case Term of
        Atom ->
            {ok, []};
        _ ->
            Err = lee_lib:format("Expected ~p, got ~p", [Atom, Term]),
            {error, [Err], []}
    end;
validate_term(Model, Type = #type{id = TypeName, parameters = Params}, Term) ->
    #mnode{ metatypes = Meta
          , metaparams = Attr1
          } = lee_model:get(TypeName, Model),
    case {ordsets:is_element(type, Meta), ordsets:is_element(typedef, Meta)} of
        {true, false} ->
            Fun = ?m_attr(type, validate, Attr1),
            Fun(Model, Type, Term);
        {false, true} ->
            Type1 = ?m_attr(typedef, type, Attr1),
            TypeVars = ?m_attr(typedef, type_variables, Attr1),
            VarVals = maps:from_list(lists:zip(TypeVars, Params)),
            Type2 = subst_type_vars(Type1, VarVals),
            validate_term(Model, Type2, Term)
    end.

%% @doc Get a value from the config:
-spec get(lee:model() | lee:cooked_module(), data(), lee:key()) -> term().
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

%% List instances that can match the pattern
-spec list(model() | cooked_module(), data(), lee:key()) -> [lee:key()].
list(_Model, Data, Pattern) when ?is_storage(Data) ->
    %% TODO: Include default values in the list
    lee_storage:list(Pattern, Data);
list(Model, Data, Pattern) when is_list(Data) ->
    lists:usort(lists:foldl( fun(I, Acc) ->
                                     list(Model, I, Pattern) ++ Acc
                             end
                           , []
                           , Data)).

%% Validate all values against the model
-spec validate(lee:model(), data()) -> validate_result().
validate(Model, Data) ->
    validate([value, map], Model, Data).

%% Validate all instances of certain metatypes against the model
-spec validate([metatype()] | all, lee:model(), data()) -> validate_result().
validate(MetaTypes, Model, Data) ->
    ModelIdx0 = Model#model.meta_class_idx,
    ModelIdx = case MetaTypes of
                   all -> ModelIdx0;
                   L   -> maps:with(L, ModelIdx0)
               end,
    {Errors, Warnings} =
        maps:fold( fun(MetaType, Nodes, {Err0, Warn0}) ->
                           {Err1, Warn1} = validate_nodes(Model, Data, MetaType, Nodes),
                           {Err1 ++ Err0, Warn1 ++ Warn0}
                   end
                 , {[], []}
                 , ModelIdx
                 ),
    case Errors of
        [] ->
            {ok, Warnings};
        _ ->
            {error, Errors, Warnings}
    end.

-spec from_string(lee:model(), lee:type(), string()) ->
                         {ok, term()} | {error, string()}.
from_string(Model, Type = #type{id = TId}, String) ->
    %% TODO: Hack
    StringT = lee_types:string(),
    case Type of
        StringT ->
            {ok, String};
        _ ->
            #mnode{metaparams = Attrs} = lee_model:get(TId, Model),
            Fun = maps:get( from_string
                          , Attrs
                          , fun(_, _, S) -> lee_lib:string_to_term(S) end
                          ),
            Fun(Model, Type, String)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Validate all nodes belonging to a metatype
-spec validate_nodes( lee:model()
                    , lee:data()
                    , atom() | integer() | tuple()
                    , ordsets:set(lee:model_key())
                    ) -> {[string()], [string()]}.
validate_nodes(Model, Data, MetaTypeId, Nodes) ->
    #model{metamodel = Meta} = Model,
    #mnode{metaparams = Attrs} = lee_model:get([metatype, MetaTypeId], Meta),
    ValidateFun = maps:get( validate_node
                          , Attrs
                          , fun(_, _, _, _) -> {[], []} end
                          ),
    ordsets:fold( fun(NodeId, {E0, W0}) ->
                          {E1, W1} = validate_node(Model, Data, NodeId, ValidateFun),
                          {E1 ++ E0, W1 ++ W0}
                  end
                , {[], []}
                , Nodes
                ).

-spec validate_node( lee:model()
                   , lee:data()
                   , lee:key()
                   , validate_callback()
                   ) -> {[string()], [string()]}.
validate_node(Model, Data, NodeId, ValidateFun) ->
    MNode = lee_model:get(NodeId, Model),
    Instances = case lee_model:split_key(NodeId) of
                    {[], _} ->
                        [NodeId];
                    {Base, Required} ->
                        [I ++ Required || I <- lee_storage:list(Base, Data)]
                end,
    lists:foldl( fun(Id, {Err0, Warn0}) ->
                         {Err1, Warn1} = ValidateFun(Model, Data, Id, MNode),
                         {Err1 ++ Err0, Warn1 ++ Warn0}
                 end
               , {[], []}
               , Instances
               ).

%% Validate nodes of `value' metatype
-spec validate_value( lee:model()
                    , lee:data()
                    , lee:key()
                    , #mnode{}
                    ) -> {[string()], [string()]}.
validate_value(Model, Data, Key, #mnode{metaparams = Attrs}) ->
    Type = maps:get(type, Attrs),
    Default = case Attrs of
                  #{default := Default0} ->
                      {ok, Default0};
                  _ ->
                      undefined
              end,
    case {lee_storage:get(Key, Data), Default} of
        {{ok, Term}, _} ->
            Result = validate_term(Model, Type, Term),
            case lee_lib:inject_error_location(Key, Result) of
                {ok, Warn} ->
                    {[], Warn};
                {error, Err, Warn} ->
                    {Err, Warn}
            end;
        {undefined, {ok, _}} ->
            {[], []};
        {undefined, undefined} ->
            Err = lee_lib:format("~p: Mandatory value is missing in the config", [Key]),
            {[Err] , []}
    end.

-spec subst_type_vars(lee:type(), map()) -> lee:type().
subst_type_vars(Atom, _) when is_atom(Atom) ->
    Atom;
subst_type_vars({var, Var}, VarVals) ->
    #{Var := Subst} = VarVals,
    Subst;
subst_type_vars(Type = #type{parameters = Params0}, VarVals) ->
    Params = [case I of
                  {var, Var} ->
                      #{Var := Subst} = VarVals,
                      Subst;
                  _ ->
                      subst_type_vars(I, VarVals)
              end || I <- Params0],
    Type#type{parameters = Params}.
