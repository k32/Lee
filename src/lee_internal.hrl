-ifndef(LEE_INTERRNAL_HRL).
-define(LEE_INTERNAL_HRL, 1).

-include("lee.hrl").

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

-endif.
