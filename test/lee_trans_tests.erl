-module(lee_trans_tests).

-lee({get_foo, [value, cli_param],
      #{ oneliner => "Foo controls fooing"
       , default => true
       }}).
-spec get_foo() -> atom().
get_foo() ->
    lee:get([blah, foo]).
