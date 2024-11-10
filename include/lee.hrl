-ifndef(LEE_HRL).
-define(LEE_HRL, true).

-define(children, {}).

-define(key_elements, key_elements).

-define(is_storage(A), (element(1, (A)) =:= lee_tree)).

%% Internal mnode definition
-record(mnode,
        { metatypes = []    :: ordsets:ordset(atom())
        , metaparams = #{}  :: map()
        }).

-record(type,
        { id                :: lee:key()
        , refinement = #{}  :: map()
        , parameters = []   :: [lee:type()]
        }).

%% (Documentation purpose) Specifies that `Code' can't crash if the
%% model has been properly validated according to the rules of
%% `MetaType'. Use this macro to keep track of things that should be
%% validated.
-define(m_valid(MetaType, Code), Code).

%% Get a mandatory attribute from the attribute map `Attrs', assuming
%% the latter is valid under `MetaType'
-define(m_attr(MetaType, Attr, Attrs), ?m_valid(MetaType, maps:get(Attr, Attrs))).

%% Get an optional attribute from the attribute map `Attrs', assuming
%% the latter is valid under `MetaType'
-define(m_attr(MetaType, Attr, Attrs, Default),
        ?m_valid(MetaType, maps:get(Attr, Attrs, Default))).

%% Takes place of a configuration value for the lee map
-define(lee_map_placeholder, []).

%% Shortcuts for lee storages that use global data (so it can be created on the statically)
-define(lee_persistent_term_storage(KEY), {lee_tree, lee_persistent_term_storage, KEY}).
-define(lee_mnesia_storage(KEY), {lee_tree, lee_mnesia_storage, KEY}).
-define(lee_dirty_mnesia_storage(KEY), {lee_tree, lee_dirty_mnesia_storage, KEY}).

-record(doclet,
        { mt :: lee:metatype()
        , tag :: atom()
        , key :: undefined | lee:model_key()
        , data
        }).

-record(doc_xref,
        { mt :: lee:metatype()
        , key :: lee:model_key()
        }).

-endif.
