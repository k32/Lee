-ifndef(LEE_INTERRNAL_HRL).
-define(LEE_INTERNAL_HRL, 1).

-record(model,
        { metamodel        :: lee:model_fragment()
        , model            :: lee:model_fragment()
        }).

-record(data,
        { %% Callback module implementing lee_storage behavior:
          backend          :: atom()
          %% Data itself:
        , data             :: term()
        }).

-record(type,
        { id               :: lee:key()
        , refinement = #{} :: map()
        , parameters = []  :: [lee:type()]
        }).

-endif.
