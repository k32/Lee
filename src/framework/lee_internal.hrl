-ifndef(LEE_INTERRNAL_HRL).
-define(LEE_INTERNAL_HRL, 1).

%% @private

-include("lee.hrl").

-define(unused, []).

-record(model,
        { metaconfig       :: lee_storage:storage(_)
        , model            :: lee:cooked_module()
        , metamodules      :: #{lee:metatype() => module()}
        , meta_class_idx   :: #{lee:metatype() => [lee:key()]}
        }).

-define(bakedin_model_key, '$bakedin_model').

-endif.
