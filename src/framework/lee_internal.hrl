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

%% (Documentation purpose) Specifies that `Code' can't crash if the
%% model has been properly validated according to the rules of
%% `MetaType'. Use this macro to keep track of things that should be
%% validated.
-define(m_valid(MetaType, Code), Code).

%% Get a mandatory attribute from the attribute map `Attrs', assuming
%% the latter is valid under `MetaType'
-define(m_attr(MetaType, Attr, Attrs), ?m_valid(MetaType, maps:get(Attr, Attrs))).

-endif.
