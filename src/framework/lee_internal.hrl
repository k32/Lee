-ifndef(LEE_INTERRNAL_HRL).
-define(LEE_INTERNAL_HRL, 1).

%% @private

-include("lee.hrl").

-define(unused, []).

-record(model,
        { metamodel        :: lee:cooked_module()
        , model            :: lee:cooked_module()
        , meta_class_idx   :: #{lee:metatype() => [lee:key()]}
        }).

-define(data_table, lee_data).
-define(model_table, lee_model).
-define(metamodel_table, lee_metamodel).

-endif.
