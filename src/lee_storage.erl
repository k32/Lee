-module(lee_storage).

-include("lee_internal.hrl").

-export([new/2]).

-export_type([ storage/0
             ]).

-type storage() :: term().

-callback create(Options :: term()) ->
    storage().

-callback get(lee:model(), storage(), lee:key()) ->
    term().

-callback put(lee:model(), storage(), [{lee:key(), term()}]) ->
    storage().

-callback list(lee:model(), storage(), lee:key()) ->
    [lee:key()].

-spec new(atom(), term()) -> lee:data().
new(Module, Options) ->
    #data{ backend = Module
         , data    = Module:create(Options)
         }.
