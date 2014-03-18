
-module(erlsc_codec).

-export([ compile/1
        , load/1
        ]).

-export([ encode/4
        ]).

-export_type([ ctx/0
             ]).

-include("erlsc_private.hrl").

-type specs() :: erlsc_specs:specs().
-type cache() :: erlsc_cache:cache().

-record(ctx, { namespace = "" :: namespace()
             , cache          :: cache()
             }).

-type ctx() :: #ctx{}.

%%%_* APIs =====================================================================

%% @doc Compile type specs according to input specs.
-spec compile(specs()) -> cache().
compile(Specs) ->
  AvscSpecs = erlsc_specs:get_avsc_specs(Specs),
  Filename  = erlsc_specs:get_erlsc_file(Specs),
  Roots     = erlsc_specs:get_roots(Specs),
  ResCache = compile(Roots,
                     _Modules = sets:new(),
                     _RawCache = erlsc_cache:init(),
                     _ResCache = erlsc_cache:init()
                    ),
  Types0    = compact_refs(ResCache),
  Types     = resolve_unions(Types0),
  ok        = erlsc_avro:pretty_print(AvscSpecs, Types),
  ok        = erlsc_types:write_to_file(Filename, Types),
  #ctx{ namespace = keyfind(namespace, AvscSpecs, "")
      , cache     = erlsc_cache:from_list(Types)
      }.

%% @doc Load compiled types from file.
-spec load(specs()) -> cache().
load(Specs) ->
  Filename  = erlsc_specs:get_erlsc_file(Specs),
  AvscSpecs = erlsc_specs:get_avsc_specs(Specs),
  Types     = erlsc_types:consult_file(Filename),
  ok        = erlsc_avro:pretty_print(AvscSpecs, Types),
  #ctx{ namespace = keyfind(namespace, AvscSpecs, "")
      , cache     = erlsc_cache:from_list(Types)
      }.

-spec encode(ctx(), root_id(), term(), [encode_opt()]) ->
        {ok, iodata()} | no_return().
encode(#ctx{namespace = Ns, cache = Cache}, Root, Data, Options) ->
  case erlsc_cache:find(Cache, Root) of
    none -> throw({unknown_root, Root});
    Type -> erlsc_avro:encode(Ns, Cache, Type, Data, Options)
  end.

%%%_* Internals ================================================================

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

%% @private Shorten the reference chain, filter out indirect references.
%% e.g. -type spec_a() :: spec_b()
%%      -type spec_b() :: #some_rec{}.
%% spec_b here is an indirect reference to type #some_rec{}, such references
%% can be ignored in the final schema representation.
%% @end
compact_refs(ResCache) ->
  compact_refs(erlsc_cache:to_list(ResCache), ResCache, []).

compact_refs([], _ResCache, Acc) ->
  %% completed
  Acc;
compact_refs([Type | Types], ResCache, Acc) ->
  %% look into sub references
  case is_named_avro_type(Type) of
    true ->
      NewType = compact_sub_refs(Type, ResCache),
      compact_refs(Types, ResCache, [NewType | Acc]);
    false ->
      compact_refs(Types, ResCache, Acc)
  end.

%% @private Union members of atom() and string() are actually mapped to
%% the same avro type 'string'. This function is to resolve the duplications.
%% @end
resolve_unions(Types) ->
  lists:map(fun resolve_unions_/1, Types).

resolve_unions_(#t{def = union, subs = Subs} = T) ->
  NewSubs = resolve_unions(Subs),
  case resolve_union(NewSubs) of
    [#t{} = NewT] -> NewT#t{id = T#t.id};
    NewSubs_      -> T#t{subs = NewSubs_}
  end;
resolve_unions_(#t{subs = Subs} = T) ->
  T#t{subs = resolve_unions(Subs)}.

%% @private Resolve string union, in the union members, there should be
%% no type duplication.
%% @end
resolve_union(Types) ->
  Suspects = [null, long, string],
  lists:foldl(fun(Suspect, Types_) ->
                resolve_union(Types_, Suspect)
              end, Types, Suspects).

resolve_union(Types, AvroDef) ->
  resolve_union(Types, AvroDef, [], false).

resolve_union([], _AvroDef, Acc, _Found) ->
  lists:reverse(Acc);
resolve_union([H | T], AvroDef, Acc, Found) ->
  case erlsc_avro:get_def(H) of
    {primitive, AvroDef} when Found =:= true ->
      %% already found, skip
      %% might be tricky for default values in the future
      assert_no_tx(H),
      resolve_union(T, AvroDef, Acc, Found);
    {primitive, string} ->
      %% found string, loop with Found = true
      assert_no_tx(H),
      resolve_union(T, AvroDef, [H#t{def = string} | Acc], true);
    {primitive, AvroDef} ->
      %% found <type>, loop with Found = true
      assert_no_tx(H),
      resolve_union(T, AvroDef, [H | Acc], true);
    _ ->
      %% not found, loop with new Acc
      resolve_union(T, AvroDef, [H | Acc], Found)
  end.

%% @private An Erlang union of integer() and string() would end up only 'string'
%% type in avro, if any of the Erlang union member is a transform result, it
%% would be impossible to resolve.
assert_no_tx(#t{ref = []}) -> ok;
assert_no_tx(T) -> throw({no_transform_allowed_in_ambiguous_union, T}).

compact_sub_refs(#t{def = [], ref = Ref} = T0, ResCache) ->
  T1 = erlsc_cache:find(ResCache, Ref),
  T = case is_named_avro_type(T1) of
        true -> T0;
        false ->
          T2 = compact_sub_refs(T1, ResCache),
          T2#t{ id  = T0#t.id
              , ref = add_ref(Ref, T2#t.ref)
              }
      end,
  filter_refs(T);
compact_sub_refs(#t{subs = Subs} = Type, ResCache) ->
  F = fun(T) -> compact_sub_refs(T, ResCache) end,
  Type#t{subs = lists:map(F, Subs)}.

is_named_avro_type(#t{def = []}) -> false;
is_named_avro_type(T)            -> erlsc_avro:is_named_type(T).

add_ref(Ref, Refs) when is_list(Refs) -> [Ref | Refs];
add_ref(Ref, Ref2)                    -> add_ref(Ref, [Ref2]).

filter_refs(#t{def = [], ref = Refs} = T) ->
  {Last, RestRefs} = take_last_ref(Refs),
  T#t{ref = filter_refs_list(RestRefs) ++ [Last]};
filter_refs(#t{ref = Refs} = T) ->
  T#t{ref = filter_refs_list(Refs)}.

take_last_ref(Ref) when is_tuple(Ref) ->
  {Ref, []};
take_last_ref(Refs) when is_list(Refs) ->
  ReversedRefs = lists:reverse(Refs),
  {hd(ReversedRefs), lists:reverse(tl(ReversedRefs))}.

filter_refs_list(Ref) when is_tuple(Ref) ->
  filter_refs_list([Ref]);
filter_refs_list(Refs) when is_list(Refs) ->
  lists:filter(fun({_, _, Arity}) -> Arity =/= 0 end, Refs).

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
