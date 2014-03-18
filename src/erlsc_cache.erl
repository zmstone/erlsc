
-module(erlsc_cache).

-export([ add/2
        , find/2
        , from_list/1
        , init/0
        , to_list/1
        ]).

-export_type([ cache/0
             ]).

-include("erlsc_private.hrl").

-type cache() :: gb_tree().

init() -> gb_trees:empty().

find(Cache, Root) ->
  case gb_trees:lookup(Root, Cache) of
    none          -> none;
    {value, Type} -> Type
  end.

add(Cache, #t{id = Root} = Type) ->
  true = ?IS_ROOT(Root), %% assert
  gb_trees:enter(Root, Type, Cache);
add(Cache, Types) when is_list(Types) ->
  lists:foldl(fun(T, C) -> add(C, T) end, Cache, Types).

to_list(Cache) ->
  lists:map(fun({_Root, Type}) -> Type end, gb_trees:to_list(Cache)).

from_list(List) ->
  gb_trees:from_orddict(
    orddict:from_list(
      lists:map(fun(#t{id = ID} = Type) -> {ID, Type} end, List))).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
