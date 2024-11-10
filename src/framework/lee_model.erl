%% @doc This module defines functions for manipulating Lee models.
-module(lee_model).

%% API exports
-export([ compile/2
        , map_vals/2

        , fold/3
        , fold/4
        , fold_metatypes/3
        , fold_mt_instances/3

        , get/2
        , get_meta/3
        , get_meta/2
        , patch_meta/2
        , all_metatypes/1
        , get_metatype_index/2
        , match/2
        , get_model_key/1
        , merge/1
        , merge/2
        , split_key/1
        , full_split_key/1
        , clone/4
        ]).

-export_type([ metatype_index/0
             ]).

-include("lee_internal.hrl").

%%====================================================================
%% Types
%%====================================================================

-type metatype_index() :: #{lee:metatype() => ordsets:ordset(lee:model_key())}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Merge multiple model and metamodel modules into a
%% machine-friendly form
-spec compile([T], [M]) -> {ok, #model{}} | {error, [string()]}
              when M :: lee:lee_module(),
                   T :: lee_metatype:cooked_metatype().
compile(MetaModules0, Models0) ->
    MetaModules = lists:flatten(MetaModules0),
    ModuleLookup = maps:from_list([{Name, Module} || {Module, Names, _Conf} <- MetaModules,
                                                     Name <- Names]),
    MetaConfigPatch = ([{set, K, V} || {_Module, _, Conf} <- MetaModules,
                                       {K, V} <- Conf]),
    MetaConfig0 = lee_storage:new(lee_map_storage),
    MetaConfig1 = lee_storage:patch(MetaConfig0, MetaConfigPatch),
    Models = [compile_module(ModuleLookup, I) || I <- Models0],
    case merge(Models) of
        {ok, Model} ->
            Result = #model{ metaconfig     = MetaConfig1
                           , model          = Model
                           , metamodules    = ModuleLookup
                           , meta_class_idx = mk_metatype_index(Model)
                           },
            case lee:meta_validate(Result) of
                {[], _Warn, Patch} ->
                    {ok, patch_meta(Result, Patch)};
                {Errs, _Warns, _Patch} ->
                    {error, Errs}
            end;
        T ->
            {error, [Err || {error, Err} <- [T]]}
    end.

-spec patch_meta(lee:model(), lee:patch()) -> lee:model().
patch_meta(M = #model{metaconfig = MC0}, Patch) ->
    MC = lee_storage:patch(MC0, Patch),
    M#model{metaconfig = MC}.

%% @doc Merge multiple Lee model modules into a single module
-spec merge([M]) -> {ok, lee:cooked_module()} | {error, term()}
               when M :: lee:cooked_module().
merge(L) ->
    M0 = lee_storage:new(lee_map_storage),
    try
        {ok, lists:foldl( fun(M1, Acc) ->
                                  case merge(M1, Acc) of
                                      {ok, M2} -> M2;
                                      Err      -> throw(Err)
                                  end
                          end
                        , M0
                        , L
                        )}
    catch
        Err -> Err
    end.

%% @doc Merge two Lee model modules into a single module
-spec merge(M, lee:lee_module()) -> {ok, M} | {error, [string()]}
              when M :: lee:cooked_module().
merge(M1, M2) ->
    Fun = fun(Key, MNode, Acc) ->
                  [{set, Key, MNode} | Acc]
          end,
    Patch = fold(Fun, [], M2),
    Collisions = [K || {set, K, _} <- Patch,
                       case lee_storage:get(K, M1) of
                           {ok, _}   -> true;
                           undefined -> false
                       end],
    case Collisions of
        [] ->
            {ok, lee_storage:patch(M1, Patch)};
        _ ->
            {error, [lee_lib:format("Colliding model key: ~p", [Key]) || Key <- Collisions]}
    end.

%% @doc Get a node from the model, assuming that it is present
-spec get(lee:model_key(), lee:model() | lee:cooked_module()) -> #mnode{}.
get(Id, #model{model = Module}) ->
    get(Id, Module);
get(Id, Module) ->
    case lee_storage:get(Id, Module) of
        {ok, Val} -> Val;
        _         -> error({bad_model_key, Id})
    end.

%% @doc Get a value from the metaconfig
-spec get_meta(lee:model_key(), lee:model(), Val) -> Val.
get_meta(Id, #model{metaconfig = MetaConfig}, Default) ->
	case lee_storage:get(Id, MetaConfig) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.

%% @doc Get a node from the metamodel
-spec get_meta(lee:model_key(), lee:model()) -> {ok, _Val} | undefined.
get_meta(Id, #model{metaconfig = MetaConfig}) ->
	lee_storage:get(Id, MetaConfig).

%% @doc Apply a function to all nodes of a raw model module.
-spec map_vals( fun((lee:model_key(), lee:mnode()) -> lee:mnode())
              , lee:lee_module()
              ) -> lee:lee_module().
map_vals(Fun, Model) ->
    do_map_vals(Fun, Model, []).

%% @doc Recursion schema for model fold
-spec fold( fun((lee:model_key(), #mnode{}, Acc) -> Acc)
          , Acc
          , Model
          ) -> Acc
      when Acc   :: term()
         , Model :: lee:model() | lee:lee_module().
fold(Fun0, Acc0, Data) ->
    Fun = fun(Key, Val, Acc, _) ->
                  {Fun0(Key, Val, Acc), ?unused}
          end,
    fold(Fun, Acc0, ?unused, Data).

%% @doc Recursion schema for model fold with scope
-spec fold( fun((lee:model_key(), #mnode{}, Acc, Scope) -> {Acc, Scope})
          , Acc
          , Scope
          , Model
          ) -> Acc
      when Model :: lee:model() | lee:lee_module() | lee:cooked_module().
fold(Fun, Acc, Scope, #model{model = Model}) -> %% Fold over cooked model
    lee_storage:fold(Fun, Acc, Scope, Model);
fold(Fun, Acc, Scope, Module) when is_map(Module) -> %% Fold over raw model
    fold([], Fun, Acc, Scope, Module);
fold(Fun, Acc, Scope, Model) -> %% Fold over cooked module
    lee_storage:fold(Fun, Acc, Scope, Model).

%% @doc Transform instance key to model key
-spec get_model_key(lee:key()) -> lee:model_key().
get_model_key([]) ->
    [];
get_model_key([Tup]) when is_tuple(Tup) ->
    [];
get_model_key([Tup | T]) when is_tuple(Tup) ->
    [?children | get_model_key(T)];
get_model_key([A|T]) ->
    [A | get_model_key(T)].

%% @doc Check whether an instance key matches a mnode key
-spec match(lee:model_key(), lee:key()) -> boolean().
match(MK, IK) ->
    get_model_key(IK) =:= MK.

%% @doc Split a key into a <i>base</i> part and a <i>required
%% part</i>. Child object with <i>required part</i> of the key is
%% mandatory if object with <i>base</i> key is present.
%%
%% Consider an example:
%% ```[a, '$children', b, '$children',c]'''
%% Here `[a]' may or may not have children, same goes for
%% `[a, X, b]'. Hence ```[a, '$children', b, $children']''' is the
%% <i>base</i> of the key and `[c]' is the <i>required part</i>.
-spec split_key(lee:model_key()) -> {lee:model_key(), lee:model_key()}.
split_key(K) ->
    {Req0, Base0} = lists:splitwith( fun(I) -> I =/= ?children end
                                   , lists:reverse(K)
                                   ),
    {lists:reverse(Base0), lists:reverse(Req0)}.

%% @doc Split a key into a list of child keys. Example:
%% ```full_split_key([foo, ?children, bar, baz, {[1]}, quux]) -->
%%        [[foo, ?children], [bar, baz, {[1]}], [quux]]'''
-spec full_split_key(lee:key()) -> [lee:key()].
full_split_key(Key) ->
    Pred = fun(T) when is_tuple(T) -> false;
              (_)                  -> true
           end,
    lee_lib:splitl(Pred, Key).

-spec all_metatypes(lee:model()) -> [lee:metatype()].
all_metatypes(#model{metamodules = MM}) ->
    maps:keys(MM).

%% @doc Get an index of mnodes belonging to metatypes
-spec get_metatype_index(lee:metatype(), lee:model()) ->
          ordsets:ordset(lee:model_key()).
get_metatype_index(MT, #model{meta_class_idx = Idx}) ->
    maps:get(MT, Idx, []).

%% @doc Copy model from one lee_storage to another
-spec clone(lee:model(), module(), map(), map()) -> lee:model().
clone(M0 = #model{metaconfig = A, model = B}, Backend, BO1, BO2) ->
    M0#model{ metaconfig = lee_storage:clone(A, Backend, BO1)
            , model = lee_storage:clone(B, Backend, BO2)
            }.

%% @doc Iterate through all metatypes
-spec fold_metatypes(fun((lee:metatype(), Acc) -> Acc), Acc, lee:model()) -> Acc.
fold_metatypes(Fun, Acc, #model{metamodules = M}) ->
    lists:foldl(Fun, Acc, maps:keys(M)).

%% @doc Iterate through all metatype instances
-spec fold_mt_instances( fun((lee:metatype(), lee:model_key(), Acc) -> Acc)
                       , Acc
                       , lee:model()
                       ) -> Acc.
fold_mt_instances(Fun, Acc0, #model{meta_class_idx = Idx}) ->
    maps:fold(
      fun(MT, Instances, Acc1) ->
              ordsets:fold( fun(Instance, Acc) -> Fun(MT, Instance, Acc) end
                          , Acc1
                          , Instances
                          )
      end,
      Acc0,
      Idx).

%%====================================================================
%% Internal functions
%%====================================================================

do_map_vals(Fun, Model, Parent) ->
    maps:map( fun(K, Val) when is_map(Val) ->
                      do_map_vals(Fun, Val, [K|Parent]);
                 (K, Node) ->
                      ParentKey = lists:reverse([K|Parent]),
                      case Node of
                          {MTs, Attrs} ->
                              Fun(ParentKey, {MTs, Attrs});
                          {MTs0, Attrs0, Children0} ->
                              Children =  do_map_vals(Fun, Children0, [{}, K|Parent]),
                              {MTs, Attrs} = Fun(ParentKey, {MTs0, Attrs0}),
                              {MTs, Attrs, Children}
                      end
              end
            , Model
            ).

-spec compile_module(_ModuleLookup, lee:lee_module() | lee:cooked_module()) -> lee:cooked_module().
compile_module(MLookup, Module) when is_map(Module) ->
    Fun = fun(Key, MNode = #mnode{metaparams = MPs0, metatypes = MTs}, Acc) ->
                  MPs = lists:foldl( fun(MT, Params) ->
                                             #{MT := MTMod} = MLookup,
                                             lee_metatype:pre_compile(MT, MTMod, Params)
                                     end
                                   , MPs0
                                   , MTs),
                  [{set, Key, MNode#mnode{metaparams = MPs}} | Acc]
          end,
    lee_storage:patch( lee_storage:new(lee_map_storage)
                     , fold(Fun, [], Module)
                     ).

%% Fold over raw model:
fold(Key0, Fun, AccIn, ScopeIn, M) when is_map(M) -> %% Namespace
    maps:fold( fun(K, Children, Acc0) ->
                       Key = Key0 ++ [K],
                       fold(Key, Fun, Acc0, ScopeIn, Children)
               end
             , AccIn
             , M
             );
fold(Key, Fun, Acc0, Scope0, MN0) ->
    case MN0 of
        {MT, MP, Children} -> ok;
        {MT, MP}           -> Children = #{}
    end,
    MNode = #mnode{ metatypes = ordsets:from_list(MT)
                  , metaparams = MP
                  },
    {Acc, Scope} = Fun(Key, MNode, Acc0, Scope0),
    fold(Key ++ [?children], Fun, Acc, Scope, Children).

%% @doc Make an index of MOCs belonging to metatypes
-spec mk_metatype_index(lee:cooked_module()) -> metatype_index().
mk_metatype_index(MF) ->
    lee_storage:fold(fun mk_metatype_index_/3, #{}, MF).

mk_metatype_index_(Key, #mnode{metatypes = MetaTypes}, Acc0) ->
    lists:foldl( fun(MT, Acc) ->
                         S0 = maps:get(MT, Acc, ordsets:new()),
                         Acc#{MT => ordsets:add_element(Key, S0)}
                 end
               , Acc0
               , MetaTypes).
