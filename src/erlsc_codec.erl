
-module(erlsc_codec).

-export([ compile/1
        , load/1
        ]).

-include("erlsc_private.hrl").

-type specs() :: erlsc_specs:specs().
-type cache() :: erlsc_cache:cache().

%%%_* APIs =====================================================================

%% @doc Compile type specs according to input specs.
-spec compile(specs()) -> cache().
compile(Specs) ->
  AvscSpecs = erlsc_specs:get_avsc_specs(Specs),
  Filename  = erlsc_specs:get_erlsc_file(Specs),
  Roots     = erlsc_specs:get_roots(Specs),
  ResCache  = compile(Roots,
                      _Modules = sets:new(),
                      _RawCache = erlsc_cache:init(),
                      _ResCache = erlsc_cache:init()
                     ),
  Types     = erlsc_avro:avro_customize(ResCache),
  ok        = erlsc_avro:pp_schema(AvscSpecs, Types),
  ok        = erlsc_types:write_to_file(Filename, Types),
  Namespace = keyfind(namespace, AvscSpecs, ""),
  Cache     = erlsc_cache:from_list(Types),
  fun(Root, Data, Options) ->
    encode(Namespace, Cache, Root, Data, Options)
  end.

%% @doc Load compiled types from file.
-spec load(specs()) -> cache().
load(Specs) ->
  Filename  = erlsc_specs:get_erlsc_file(Specs),
  AvscSpecs = erlsc_specs:get_avsc_specs(Specs),
  Types     = erlsc_types:consult_file(Filename),
  ok        = erlsc_avro:pretty_print(AvscSpecs, Types),
  Namespace = keyfind(namespace, AvscSpecs, ""),
  Cache     = erlsc_cache:from_list(Types),
  fun(Root, Data, Options) ->
    encode(Namespace, Cache, Root, Data, Options)
  end.

%%%_* Internals ================================================================

-spec encode(namespace(), cache(), root_id(), term(), [encode_opt()]) ->
        {binary(), binary()} | no_return().
encode(Ns, Cache, Root, Data, Options) ->
  case erlsc_cache:find(Cache, Root) of
    none -> throw({unknown_root, Root});
    Type -> erlsc_avro:encode(Ns, Cache, Type, Data, Options)
  end.

%% @private Compile all the given spec roots.
-spec compile([root_id()], set(), cache(), cache()) ->
        cache() | no_return().
compile([], _Modules, _RawCache, ResCache) ->
  %% all types compiled and references are resolved
  %% compact direct references to non-named types
  ResCache;
compile([{Module, _, _} = Root | Roots], Modules, RawCache, ResCache) ->
  case check(Root, Modules, RawCache, ResCache) of
    continue ->
      compile(Roots, Modules, RawCache, ResCache);
    {resolve_refs, #t{def = {nok, Reason}}} ->
      throw(Reason);
    {resolve_refs, Type} ->
      %% resolve references
      NewRoots = find_references(Type, []),
      NewResCache = erlsc_cache:add(ResCache, Type),
      compile(NewRoots ++ Roots, Modules, RawCache, NewResCache);
    compile ->
      Types       = erlsc_types:compile(Module),
      NewRawCache = erlsc_cache:add(RawCache, Types),
      NewModules  = sets:add_element(Module, Modules),
      compile([Root | Roots], NewModules, NewRawCache, ResCache)
  end.

%% @private Find all reference roots of a given type.
-spec find_references(type(), [root_id()]) -> [root_id()].
find_references(#t{ref = Root, subs = Subs}, Roots) when ?IS_ROOT(Root) ->
  []   = Subs,
  [Root | Roots];
find_references(#t{subs = Subs}, Roots) ->
  lists:foldl(fun find_references/2, Roots, Subs).

%% @private Check what to do for the given type root.
-spec check(root_id(), set(), cache(), cache()) ->
        continue | compile | {resolve_refs, type()} | no_return().
check({Module, _, _} = Root, Modules, RawCache, ResCache) ->
  case erlsc_cache:find(ResCache, Root) of
    none ->
      case erlsc_cache:find(RawCache, Root) of
        none ->
          sets:is_element(Module, Modules) andalso throw({badarg, Root}),
          compile;
        Type ->
          {resolve_refs, Type}
      end;
    _Type ->
      continue
  end.

keyfind(Key, KVL, Default) ->
  case lists:keyfind(Key, 1, KVL) of
    {_Key, Val} -> Val;
    false       -> Default
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
