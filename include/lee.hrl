-ifndef(LEE_HRL).
-define(LEE_HRL, true).

-record(type,
        { id               :: lee:key()
        , refinement = #{} :: map()
        , parameters = []  :: [lee:type()]
        }).

-endif.
