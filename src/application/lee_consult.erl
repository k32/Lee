-module(lee_consult).

-export([ metamodel/0
        , read/2
        , read/3
        , read_to/3
        , read_to/4
        ]).

-export_type([filter/0]).

-include("lee.hrl").

-define(consult, consult).

-type filter() :: [lee:metatype()] | all.

-spec metamodel() -> lee:module().
metamodel() ->
    #{ metatype => #{ ?consult => {[metatype]
                                  , #{}
                                  }
                    }}.

%% @doc Parse file into a `lee_storage'
%% @throws {error, string()}
-spec read_to(lee:model(), file:filename(), lee_storage:data()) ->
                     lee_storage:data().
read_to(Model, Filename, Data) ->
    read_to(Model, Filename, all, Data).

%% @doc Parse file into a `lee_storage'
%% @throws {error, string()}
-spec read_to(lee:model(), file:filename(), filter(), lee_storage:data()) ->
                     lee_storage:data().
read_to(Model, Filename, Filter, Data) ->
    Patch = read(Model, Filename, Filter),
    lee_storage:patch(Data, Patch).

%% @doc Parse file into a patch
%% @throws {error, string()}
-spec read(lee:model(), file:filename()) -> lee:patch().
read(Model, Filename) ->
    read(Model, Filename, all).

%% @doc Parse file into a patch
%% @throws {error, string()}
-spec read(lee:model(), file:filename(), filter()) -> lee:patch().
read(Model, Filename, Filter) ->
    Predicate = case Filter of
                    all ->
                        fun(_) -> true end;
                    _ ->
                        OrdSet = ordsets:from_list(Filter),
                        fun(MetaTypes) ->
                                ordsets:intersection(OrdSet, MetaTypes) =/= []
                        end
                end,
    Keys = lee_model:get_metatype_index(?consult, Model),
    Terms0 = case file:consult(Filename) of
                 {ok, T0} ->
                     T0;
                 {error, Reason0} ->
                     Reason = lee_lib:format( "Reading ~s failed: ~p"
                                            , [Filename, Reason0]),
                     throw({error, Reason})
             end,
    case Terms0 of
         [Terms] when is_map(Terms) ->
             ok;
         _ ->
             Terms = try maps:from_list(Terms0)
                     catch
                         _:_ ->
                             throw({error, Filename ++ " should be a proplist or a map"})
                     end
    end,
    lists:foldl( fun(Key, Acc) ->
                         read_val(Model, Predicate, Terms, Key, Acc)
                 end
               , []
               , Keys).

read_val(Model, Predicate, Terms, Key, Acc) ->
    #mnode{metatypes = MT, metaparams = Attrs} = lee_model:get(Key, Model),
    Valid = Predicate(MT),
    FileKey = maps:get(file_key, Attrs),
    case Terms of
        #{FileKey := Val} when Valid ->
            [{set, Key, Val} | Acc];
        #{} ->
            Acc
    end.
