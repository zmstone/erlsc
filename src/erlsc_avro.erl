%% From erlsc schema to avro schema
%% [ref] http://avro.apache.org/docs/current/spec.html
%% LIMITATIONs:
%%   * no avro 'int' type support, always use 'long' instead
%%   * no avro 'float' type support, always use 'double' instead
%%   * no avro Maps type support
%%   * no avro Fixed type support
%%   * no default value support
%%   * record field names in Erlang spec should contain [A-Za-z0-9_] only

-module(erlsc_avro).

-export([ avro_customize/1
        , encode/5
        , pp_schema/2
        ]).

-include("erlsc_private.hrl").

-define(INDENT_WIDTH, 4).

%%%_* API ======================================================================

%% @doc Pretty print avro schema.
pp_schema(Specs, Types) ->
  OutputDir = keyfind(output_dir, Specs),
  Namespace = keyfind(namespace, Specs, ""),
  filelib:ensure_dir(filename:join(OutputDir, "foo")),
  lists:foreach(fun(Type) ->
                  pp_schema(OutputDir, Namespace, Type)
                end, Types).

%% @doc Encode Erlang term into either JSON objct or avro binary.
encode(Ns, Cache, #t{id = ID} = Type, Data, Options) ->
  case lists:member(json, Options) of
    true ->
      Json = mochijson3:encode(encode_json(Ns, Ns, Cache, Type, Data)),
      {bin(ns(Ns, ID)), bin(Json)};
    false ->
      case lists:member(binary, Options) of
        true  -> encode_binary(Ns, Cache, Type, Data);
        false -> throw({bad_options, {"no encode type", Options}})
      end
  end.

%% @doc Convert into avro specific layout.
%% 1. compact references, see compact_refs/3
%% 2. resolve ambiguous unions, see resolve_unions/1
%% @end
avro_customize(Cache) ->
  resolve_unions(compact_refs( erlsc_cache:to_list(Cache), Cache, [])).

%%%_* Compact references =======================================================

%% @private Shorten the reference chain, filter out indirect references.
%% e.g. -type spec_a() :: spec_b()
%%      -type spec_b() :: #some_rec{}.
%% spec_b here is an indirect reference to type #some_rec{}, such references
%% can be ignored in the final schema representation.
%% @end
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
is_named_avro_type(T)            -> is_named(def(T)).

%% @private Concatenate references as list.
add_ref(Ref, Refs) when is_list(Refs) -> [Ref | Refs];
add_ref(Ref, Ref2)                    -> add_ref(Ref, [Ref2]).

%% @private Keep only transform references and the last in the list
%% as that one would be needed to format avro type name.
%% @end
filter_refs(#t{def = [], ref = Refs} = T) ->
  {Last, RestRefs} = take_last_ref(Refs),
  T#t{ref = filter_refs_list(RestRefs) ++ [Last]};
filter_refs(#t{ref = Refs} = T) ->
  T#t{ref = filter_refs_list(Refs)}.

%% @private Take the last reference in the list.
take_last_ref(Ref) when is_tuple(Ref) ->
  {Ref, []};
take_last_ref(Refs) when is_list(Refs) ->
  ReversedRefs = lists:reverse(Refs),
  {hd(ReversedRefs), lists:reverse(tl(ReversedRefs))}.

%% @private Discard all the intermediate nonsense type references.
filter_refs_list(Ref) when is_tuple(Ref) ->
  filter_refs_list([Ref]);
filter_refs_list(Refs) when is_list(Refs) ->
  lists:filter(fun({_, _, Arity}) -> Arity =/= 0 end, Refs).

%%%_* Resolve avro unions ======================================================

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
  case get_def(H) of
    {primitive, AvroDef} when Found =:= true ->
      %% already found, skip.  we may lose #t.args here
      %% e.g. the Erlang union "-type some_type() :: unknown | integer()." is
      %%      resolved into 'string' type in avro, thus we lose the ability to
      %%      validate the input data by checking I == unknown or is_integer(I)
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

%%%_* Enocde JSON ==============================================================

%% @private Encode Erlang term into JSON objects.
encode_json(Ns, FullName, Cache, #t{id = ID, ref = Refs} = Type, Data) ->
  NewData = tx(Refs, Data),
  case is_instance(Type, NewData) of
    true     -> ok;
    TypeInfo -> throw({bad_data, FullName, TypeInfo, NewData})
  end,
  case get_def(Type) of
    {reference, Ref} ->
      RefType = erlsc_cache:find(Cache, Ref),
      encode_json(Ns, Ns, Cache, RefType, NewData);
    {primitive, AvroDef} ->
      encode_primitive_json(AvroDef, NewData);
    {complex, AvroDef} ->
      encode_complex_json(AvroDef, Ns, ns(FullName, ID), Cache, Type, NewData)
  end.

encode_complex_json(array, Ns, FullName, Cache, #t{subs = [T]}, Data) ->
  true = is_list(Data), %% assert
  lists:map(fun(D) -> encode_json(Ns, FullName, Cache, T, D) end, Data);
encode_complex_json(enum, _Ns, _FullName, _Cache, #t{}, Data) ->
  true = is_atom(Data),
  Data;
encode_complex_json(record, Ns, FullName, Cache, T, Data) ->
  NewData =
    case T#t.id of
      {_Mod, record, _Name} -> tl(tuple_to_list(Data));
      _                     -> tuple_to_list(Data)
    end,
  Fields =
    lists:map(fun({#t{id = ID} = SubT, D}) ->
                {ID, encode_json(Ns, FullName, Cache, SubT, D)}
              end, lists:zip(T#t.subs, NewData)),
  {struct, Fields};
encode_complex_json(union, Ns, FullName, Cache, #t{subs = Subs}, Data) ->
  Fun = fun(T) ->
          Json = encode_json(Ns, FullName, Cache, T, Data),
          case get_avro_type_name(T, Ns, FullName) of
            null -> null;
            Name -> {struct, [{Name, Json}]}
          end
        end,
  case try_each_take_first(Fun, Subs) of
    {ok, Result} -> Result;
    error        -> throw({bad_data, FullName, Subs, Data})
  end.

%% @private Try each funcion ignore exception ones until the first success.
try_each_take_first(_Fun, []) ->
  error;
try_each_take_first(Fun, [H | T]) ->
  try
    {ok, Fun(H)}
  catch throw : {bad_data, _FullName, _TypeInfo, _Data} ->
    try_each_take_first(Fun, T)
  end.

get_avro_type_name(#t{} = Type, Ns, FullName) ->
  case get_def(Type) of
    {primitive, Def}    -> Def;      %% primitive type name
    {complex, _AvroDef} -> FullName; %% complex type name
    {reference, Ref}    -> ns(Ns, Ref)
  end.

encode_primitive_json(null, _)    -> null;
encode_primitive_json(boolean, B) -> B;
encode_primitive_json(double, D)  -> D;
encode_primitive_json(long, L)    -> L;
encode_primitive_json(string, S)  -> bin(S).

%% @private Check if data is an instance of given type.
is_instance(#t{def = []}, _Data) ->
  true;
is_instance(#t{def = string}, X) ->
  is_atom(X) orelse
  is_list(X) orelse
  is_binary(X) orelse
  is_integer(X) orelse string;
is_instance(Type, Data) ->
  erlsc_types:is_instance(Data, Type).

%%%_* Enocde binary ============================================================

encode_binary(_Ns, _Cache, _Type, _Data) ->
  ok.

%%%_* Enocde binary ============================================================

%% @private Pretty print a type to JSON object and write it to .avsc file.
pp_schema(OutputDir, Ns, #t{id = ID} = Type) ->
  Filename = filename:join(OutputDir, root_id_to_filename(ID)),
  PropList = to_propl(Type, Ns, Ns),
  JSON     = pp_schema_json(_Indent = "", PropList),
  ok       = file:write_file(Filename, JSON).

to_propl(#t{} = Type, FullName, Ns) ->
  case get_def(Type) of
    {reference, Ref} ->
      json_str(ns(Ns, Ref));
    {primitive, Def} ->
      json_str(Def);
    {complex, Def} ->
      try
        complex_to_propl(Def, Type, FullName, Ns)
      catch throw : {bad_avro_name, BadID} ->
        throw({incompatible_avro_name, {Type, BadID}})
      end
  end.

complex_to_propl(Def, #t{id = ID} = Type, FullName, Ns) ->
  NewFullName = ns(FullName, ID),
  case Def of
    array  -> array_to_propl(Type, NewFullName, Ns);
    enum   -> enum_to_propl(Type, NewFullName);
    record -> record_to_propl(Type, NewFullName, Ns);
    union  -> union_to_propl(Type, NewFullName, Ns)
  end.

array_to_propl(#t{subs = [ElemType]}, FullName, Ns) ->
  [ {type, array}
  , {items, to_propl(ElemType, FullName, Ns)}
  ].

enum_to_propl(#t{args = Symbols}, FullName) ->
  [ {name, json_str(FullName)}
  , {type, enum}
  , {symbols, bin([ "["
                  , infix([json_str(A) || A <- Symbols], ",")
                  , "]"])}
  ].

record_to_propl(#t{subs = Fields}, FullName, Ns) ->
  [ {name, json_str(FullName)}
  , {type, record}
  , {fields, [record_field_to_propl(Field, FullName, Ns) || Field <- Fields]}
  ].

record_field_to_propl(#t{id = ID} = Type, FullName, Ns) ->
  [ {name, ID}
  , {type, to_propl(Type, FullName, Ns)}
  ].

union_to_propl(#t{subs = Members}, FullName, Ns) ->
  [to_propl(Member, FullName, Ns) || Member <- lists:sort(Members)].

pp_schema_json(Indent, Value) when is_binary(Value) ->
  [Indent, Value];
pp_schema_json(Indent, [H | _] = TaggedValues) when is_tuple(H) ->
  NewIndent = Indent ++ indent(),
  [ [Indent, "{\n"]
  , infix([ [ NewIndent
            , [ json_str(Tag), ":"
              , case is_list(Value) of
                  true  -> ["\n", pp_schema_json(NewIndent, Value)];
                  false -> [" ", json_str(Value)]
                end
              ]
            ] || {Tag, Value} <- TaggedValues
          ], ",\n"), "\n"
  , [Indent, "}"]
  ];
pp_schema_json(Indent, [H | _] = Union) when is_list(H);
                                             is_binary(H) ->
  [ [Indent, "[\n"]
  , infix([ pp_schema_json(Indent ++ indent(), Member)
          || Member <- Union], ",\n"), "\n"
  , [Indent, "]"]
  ].

%% @private Quote json string, use binary as an indicator of already
%% quoted string. Since we are formating avro schema here, all the strings
%% are from erlang specs, here we assume it's always either atom or string
%% which is all latin letters without any punctuation (well, except for '.')
%% @end
json_str(A) when is_atom(A)   -> json_str(atom_to_list(A));
json_str(S) when is_list(S)   -> bin(["\"", S, "\""]);
json_str(B) when is_binary(B) -> B. %% already quoted

%% @private Make a white space padding string as indentation.
indent() -> lists:duplicate(?INDENT_WIDTH, $\s).

%% @private Infix string list.
infix([], _Sep)     -> [];
infix([H], _Sep)    -> [H];
infix([H | T], Sep) -> [H, Sep | infix(T, Sep)].

%%%_* Common help functions ====================================================

%% @private Make new namespace.
-spec ns(namespace(), type_id()) -> namespace().
ns(Namespace, ID) -> string:join([Namespace, id_to_name(ID)], ".").

%% @private Get avro definition.
-spec get_def(type()) -> {reference, root_id()}        |
                         {primitive, avro_primitive()} |
                         {complex, avro_complex()}.
get_def(#t{def = [], ref = Ref}) ->
  case is_list(Ref) of
    true  -> {reference, hd(lists:reverse(Ref))};
    false -> {reference, Ref}
  end;
get_def(#t{} = T) ->
  AvroDef = def(T),
  case is_primitive(AvroDef) of
    true ->
      {primitive, AvroDef};
    false ->
      true = is_complex(AvroDef), %% assert
      {complex, AvroDef}
  end.

%% @private Map to avro type definition.
-spec def(type()) -> def() | no_return().
def(#t{def = atom, args = nil})      -> null;
def(#t{def = atom, args = null})     -> null;
def(#t{def = atom, args = undefine}) -> null;
def(#t{def = arity})                 -> long;
def(#t{def = byte})                  -> long;
def(#t{def = float})                 -> double;
def(#t{def = boolean})               -> boolean;
def(#t{def = atom, args = []})       -> string;
def(#t{def = integer})               -> string;
def(#t{def = iodata})                -> string;
def(#t{def = iolist})                -> string;
def(#t{def = module})                -> string;
def(#t{def = neg_integer})           -> string;
def(#t{def = non_neg_integer})       -> string;
def(#t{def = pos_integer})           -> string;
def(#t{def = string})                -> string;
def(#t{def = record})                -> record;
def(#t{def = tuple})                 -> record;
def(#t{def = union})                 -> union;
def(#t{def = enum})                  -> enum;
def(#t{def = L}) when ?IS_LIST(L)    -> array;
def(#t{def = range, args = Args}) ->
  {Min, Max} = erlsc_types:range(Args),
  case Min >= ?INT_MIN andalso Max =< ?INT_MAX of
    true  -> long;
    false -> string
  end;
def(T) ->
  throw({incompatible, T}).

is_named(enum)   -> true;
is_named(record) -> true;
is_named(_)      -> false.

is_primitive(boolean) -> true;
is_primitive(double)  -> true;
is_primitive(long)    -> true;
is_primitive(null)    -> true;
is_primitive(string)  -> true;
is_primitive(_)       -> false.

is_complex(array)  -> true;
is_complex(enum)   -> true;
is_complex(record) -> true;
is_complex(union)  -> true;
is_complex(_)      -> false.

keyfind(Key, KVL) ->
  case lists:keyfind(Key, 1, KVL) of
    {_Key, Val} -> Val;
    false       -> throw({not_found, {Key, KVL}})
  end.

keyfind(Key, KVL, Default) ->
  case lists:keyfind(Key, 1, KVL) of
    {_Key, Val} -> Val;
    false       -> Default
  end.

%% @private Type root ID to avro type name.
root_id_to_filename(Id) ->
  id_to_name(Id) ++ ".avsc".

%% @private Type ID to avro type name.
id_to_name({Module, record, Name}) when is_atom(Name) ->
  atom_to_list(Module) ++ ".record." ++ atom_to_list(Name);
id_to_name({Module, Name, Arity}) when is_integer(Arity) ->
  atom_to_list(Module) ++ "." ++
  atom_to_list(Name) ++ "_" ++
  integer_to_list(Arity);
id_to_name(Id) when is_atom(Id) ->
  atom_to_list(Id);
id_to_name(Id) when is_integer(Id) ->
  "_" ++ integer_to_list(Id).

%% @private Transform data using the transform {M, F, 1} functions in #t.ref.
tx([], Data)                     -> Data;
tx([{Mod, Fun, 1} | Rest], Data) -> tx(Rest, Mod:Fun(Data));
tx([_Ref | Rest], Data)          -> tx(Rest, Data).

bin(A) when is_atom(A)    -> bin(atom_to_list(A));
bin(I) when is_integer(I) -> bin(integer_to_list(I));
bin(IoList)               -> iolist_to_binary(IoList).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
