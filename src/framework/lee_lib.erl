%% @private
-module(lee_lib).

-include("lee.hrl").

-export([ term_to_string/1
        , format/2
        , patch_key/1
        , make_nested_patch/3
        , splitl/2
        , splitr/2
        , inject_error_location/2
        , compose_checks/1
        , perform_checks/3
        , run_cmd/2
        , validate_optional_meta_attr/4
        , validate_optional_meta_attr/3
        , validate_meta_attr/3
        , format_typerefl_error/1
        , report_error/2
        , report_warning/2
        , collect_errors/1
        ]).

-export_type([check_result/0]).

-type check_result() :: {[string()], [string()]}.

-spec format(string(), [term()]) -> string().
format(Fmt, Attrs) ->
    lists:flatten(io_lib:format(Fmt, Attrs)).

-spec patch_key(lee:patch_op()) -> lee:key().
patch_key({set, K, _}) ->
    K;
patch_key({rm, K}) ->
    K.

-spec make_nested_patch(lee:model(), lee:key(), #{lee:key() => term()}) ->
                               lee:patch().
make_nested_patch(_Model, [], Children) ->
    [{set, K, V} || {K, V} <- maps:to_list(Children)];
make_nested_patch(Model, Parent, Children) ->
    #mnode{metaparams = MAttrs} = lee_model:get(Parent, Model),
    KeyElems = ?m_attr(map, ?key_elements, MAttrs, []),
    MakeChildKey =
        fun(K) ->
                case Children of
                    #{K := Val} ->
                        Val;
                    _ ->
                        InstKey = Parent ++ [?children | K],
                        MNode = lee_model:get(InstKey, Model),
                        case MNode#mnode.metaparams of
                            #{default := Default} ->
                                Default;
                            _ ->
                                throw(lee_lib:format("missing key element ~p", [K]))
                        end
                end
        end,
    ChildKey = list_to_tuple(lists:map(MakeChildKey, KeyElems)),
    [{set, Parent ++ [ChildKey], ?lee_map_placeholder}
    |[{set, Parent ++ [ChildKey|K], V} || {K, V} <- maps:to_list(Children)]].

-spec splitl(fun((A) -> boolean()), [A]) -> [[A]].
splitl(_, []) ->
    [];
splitl(Pred, L) ->
    case lists:splitwith(Pred, L) of
        {[], [B | C]} ->
            [[B] | splitl(Pred, C)];
        {A, []} ->
            [A];
        {A, [B | C]} ->
            [A ++ [B] | splitl(Pred, C)]
    end.

-spec splitr(fun((A) -> boolean()), [A]) -> [[A]].
splitr(_, []) ->
    [];
splitr(Pred, L) ->
    case lists:splitwith(Pred, L) of
        {[], [B|C]} ->
            splitr(Pred, B, C);
        {A, []} ->
            [A];
        {A, [B|C]} ->
            [A|splitr(Pred, B, C)]
    end.

splitr(_, H, []) ->
    [[H]];
splitr(Pred, H, L) ->
    case lists:splitwith(Pred, L) of
        {A, []} ->
            [[H|A]];
        {A, [B|C]} ->
            [[H|A] | splitr(Pred, B, C)]
    end.

-spec term_to_string(term()) -> string().
term_to_string(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> Term;
        false -> format("~p", [Term])
    end.

-spec inject_error_location(lee:key(), check_result()) ->
                                   check_result().
inject_error_location(Location, {Err, Warn}) ->
    Fun = fun(Msg) ->
                  format( "~p: ~s", [Location, term_to_string(Msg)])
          end,
    {[Fun(I) || I <- Err], [Fun(I) || I <- Warn]}.

-spec run_cmd(string(), [string()]) -> {integer(), binary()} | {error, term()}.
run_cmd(Cmd, Args) ->
    case os:find_executable(Cmd) of
        false ->
            {error, enoent};
        Executable ->
            Port = erlang:open_port( {spawn_executable, Executable}
                                   , [ exit_status
                                     , stderr_to_stdout
                                     , {args, Args}
                                     ]),
            collect_port_output(Port, [])
    end.

collect_port_output(Port, Acc) ->
    receive
        {Port, {exit_status, Status}} ->
            {Status, iolist_to_binary(Acc)};
        {Port, {data, Data}} ->
            [Acc|Data]
    end.

-spec compose_checks([check_result()]) -> check_result().
compose_checks(L) ->
    {Err, Warn} = lists:unzip(L),
    {lists:append(Err), lists:append(Warn)}.

%% Should be called inside collect_errors
-spec report_error(string(), list()) -> ok.
report_error(Fmt, Args) ->
    Str = format(Fmt, Args),
    self() ! {get(lee_lib_collect_errors), {error, Str}},
    ok.

%% Should be called inside collect_errors
-spec report_warning(string(), list()) -> ok.
report_warning(Fmt, Args) ->
    Str = format(Fmt, Args),
    self() ! {get(lee_lib_collect_errors), {warning, Str}},
    ok.

-spec collect_errors(fun(() -> Ret)) -> {Ret, {Err, Warn}}
              when Err :: [string()],
                   Warn :: [string()].
collect_errors(Fun) ->
    PDKey = lee_lib_collect_errors,
    Ref = make_ref(),
    OldRef = put(PDKey, Ref),
    Ret = Fun(),
    case OldRef of
        undefined -> erase(PDKey);
        _         -> put(PDKey, OldRef)
    end,
    {Ret, receive_errors(Ref, [], [])}.

-spec perform_checks(lee:key(), M, [fun((M) -> check_result())]) ->
                            check_result().
perform_checks(Key, Attrs, CheckFuns) ->
    CheckResults = [F(Attrs) || F <- CheckFuns],
    inject_error_location(Key, compose_checks(CheckResults)).

-spec validate_optional_meta_attr( atom()
                                 , typerefl:type()
                                 , lee:properties() | #mnode{}
                                 , boolean()
                                 ) -> lee:validate_result().
validate_optional_meta_attr(Attr, Type, #mnode{metaparams = MP}, WarnIfAbsent) ->
    validate_optional_meta_attr(Attr, Type, MP, WarnIfAbsent);
validate_optional_meta_attr(Attr, Type, Params, WarnIfAbsent) ->
    case Params of
        #{Attr := _} ->
            validate_meta_attr(Attr, Type, Params);
        _ when WarnIfAbsent ->
            Warn = format("Missing attribute ~p", [Attr]),
            {[], [Warn]};
        _ ->
            {[], []}
    end.

-spec validate_optional_meta_attr( atom()
                                 , typerefl:type()
                                 , lee:properties() | #mnode{}
                                 ) -> lee:validate_result().
validate_optional_meta_attr(Attr, Type, #mnode{metaparams = MP}) ->
    validate_optional_meta_attr(Attr, Type, MP, false).

-spec validate_meta_attr( atom()
                        , typerefl:type()
                        , lee:properties() | #mnode{}
                        ) -> lee:validate_result().
validate_meta_attr(Attr, Type, #mnode{metaparams = MP}) ->
    validate_meta_attr(Attr, Type, MP);
validate_meta_attr(Attr, Type, Params) ->
    case Params of
        #{Attr := Val} ->
            case typerefl:typecheck(Type, Val) of
                ok ->
                    {[], []};
                {error, Err} ->
                    {[format_typerefl_error(Err)], []}
            end;
        _ ->
            Err = format("Missing mandatory meta-parameter ~p", [Attr]),
            {[Err], []}
    end.

format_typerefl_error(#{expected := Expected, got := Got}) ->
    format("Type mismatch.~nExpected type: ~s~nGot value: ~p", [Expected, Got]).

receive_errors(Ref, Err, Warn) ->
    receive
        {Ref, {error, A}} ->
            receive_errors(Ref, [A|Err], Warn);
        {Ref, {warning, A}} ->
            receive_errors(Ref, Err, [A|Warn])
    after 0 ->
            {Err, Warn}
    end.
