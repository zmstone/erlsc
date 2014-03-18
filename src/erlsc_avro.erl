%% From erlsc schema to avro schema
%% [ref] http://avro.apache.org/docs/current/spec.html
%% LIMITATIONs:
%%   * no avro 'int' type support, always use 'long' instead
%%   * no avro 'float' type support, always use 'double' instead
%%   * no avro Maps type support
%%   * no avro Fixed type support
%%   * no default value support
%%   * record field names in Erlang spec should contain [A-Za-z0-9_] only
%%     e.g. {mod, record, rec} -> "mod_record_rec"
%%          {mod, mytype, 0}   -> "mod_mytype_0"

-module(erlsc_avro).

-export([ encode/5
        , is_named_type/1
        , pretty_print/2
        , get_def/1
        ]).

-include("erlsc_private.hrl").

-define(INDENT_WIDTH, 4).

%%%_* API ======================================================================

%% @doc Is named type
is_named_type(T) -> is_named(def(T)).

pretty_print(Specs, Types) ->
  OutputDir = keyfind(output_dir, Specs),
  Namespace = keyfind(namespace, Specs, ""),
  filelib:ensure_dir(filename:join(OutputDir, "foo")),
  lists:foreach(fun(Type) ->
                  pretty_print(OutputDir, Namespace, Type)
                end, Types).

encode(Ns, Cache, Type, Data, Options) ->
  case lists:member(json, Options) of
    true ->
      iolist_to_binary(
        mochijson3:encode(
          encode_json(Ns, Ns, Cache, Type, Data)));
    false ->
      case lists:member(binary, Options) of
        true  -> encode_binary(Ns, Cache, Type, Data);
        false -> throw({bad_options, {"no encode type", Options}})
      end
  end.

%%%_* Internals ================================================================

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
          Name = get_avro_type_name(Cache, T, Ns, FullName),
          case Name of
            null -> null;
            _    -> {struct, [{Name, Json}]}
          end
        end,
  case try_each_take_first(Fun, Subs) of
    {ok, Result} -> Result;
    error        -> throw({bad_data, FullName, Subs, Data})
  end.

try_each_take_first(_Fun, []) ->
  error;
try_each_take_first(Fun, [H | T]) ->
  try
    {ok, Fun(H)}
  catch throw : {bad_data, _FullName, _TypeInfo, _Data} ->
    try_each_take_first(Fun, T)
  end.

get_avro_type_name(Cache, #t{} = Type, Ns, FullName) ->
  case get_def(Type) of
    {primitive, Def}    -> Def;      %% primitive type name
    {complex, _AvroDef} -> FullName; %% complex type name
    {reference, Ref}    ->
      RefType = erlsc_cache:find(Cache, Ref),
      get_avro_type_name(Cache, RefType, Ns, ns(Ns, Ref))
  end.

encode_primitive_json(null, _)    -> null;
encode_primitive_json(boolean, B) -> B;
encode_primitive_json(double, D)  -> D;
encode_primitive_json(long, L)    -> L;
encode_primitive_json(string, S)  -> {json, quote(S)}.

quote(I) when is_integer(I) -> quote(integer_to_list(I));
quote(A) when is_atom(A)    -> quote(atom_to_list(A));
quote(S)                    -> bin([$", escape(S), $"]).

escape([])       -> [];
escape([$" | S]) -> [$\\, $" | escape(S)];
escape([H | S])  -> [H | escape(S)].

is_instance(#t{def = []}, _Data) -> true;
is_instance(#t{def = string}, X) -> is_atom(X)    orelse
                                    is_list(X)    orelse
                                    is_binary(X)  orelse
                                    is_integer(X) orelse
                                    {bad_data, string, X};
is_instance(Type, Data)          -> erlsc_types:is_instance(Data, Type).

encode_binary(_Ns, _Cache, _Type, _Data) ->
  ok.

tx([], Data)                     -> Data;
tx([{Mod, Fun, 1} | Rest], Data) -> tx(Rest, Mod:Fun(Data));
tx([_Ref | Rest], Data)          -> tx(Rest, Data).

%% @private Pretty print a type to JSON object and write it to .avsc file.
pretty_print(OutputDir, Namespace, #t{id = ID} = Type) ->
  Filename = filename:join(OutputDir, root_id_to_filename(ID)),
  JSON = pp(Type, Namespace),
  ok = file:write_file(Filename, JSON).

%% @private Pretty print a type to JSON object.
pp(Type, Ns) ->
  PropList = pp_type(Type, Ns, Ns),
  pp_json(_Indent = "", PropList).

pp_type(#t{} = Type, FullName, Ns) ->
  case get_def(Type) of
    {reference, Ref} ->
      fmt([Ns, ".", id_to_name(Ref)]);
    {primitive, Def} ->
      fmt(Def);
    {complex, Def} ->
      try
        pp_complex(Def, Type, FullName, Ns)
      catch throw : {bad_avro_name, BadID} ->
        throw({incompatible_avro_name, {Type, BadID}})
      end
  end.

pp_complex(Def, #t{id = ID} = Type, FullName, Ns) ->
  NewFullName = ns(FullName, ID),
  case Def of
    array  -> pp_array(Type, NewFullName, Ns);
    enum   -> pp_enum(Type, NewFullName);
    record -> pp_record(Type, NewFullName, Ns);
    union  -> pp_union(Type, NewFullName, Ns)
  end.

pp_array(#t{subs = [ElemType]}, FullName, Ns) ->
  [ {type, array}
  , {items, pp_type(ElemType, FullName, Ns)}
  ].

pp_enum(#t{args = Symbols}, FullName) ->
  [ {name, fmt(FullName)}
  , {type, enum}
  , {symbols, bin([ "["
                  , infix([fmt(A) || A <- Symbols], ",")
                  , "]"])}
  ].

pp_record(#t{subs = Fields}, FullName, Ns) ->
  [ {name, fmt(FullName)}
  , {type, record}
  , {fields, [pp_record_field(Field, FullName, Ns) || Field <- Fields]}
  ].

pp_record_field(#t{id = ID} = Type, FullName, Ns) ->
  [ {name, ID}
  , {type, pp_type(Type, FullName, Ns)}
  ].

pp_union(#t{subs = Members}, FullName, Ns) ->
  [pp_type(Member, FullName, Ns) || Member <- lists:sort(Members)].

pp_json(Indent, Value) when is_binary(Value) ->
  [Indent, Value];
pp_json(Indent, [H | _] = TaggedValues) when is_tuple(H) ->
  NewIndent = Indent ++ indent(),
  [ [Indent, "{\n"]
  , infix([ [ NewIndent
            , [ fmt(Tag), ":"
              , case is_list(Value) of
                  true  -> ["\n", pp_json(NewIndent, Value)];
                  false -> [" ", fmt(Value)]
                end
              ]
            ] || {Tag, Value} <- TaggedValues
          ], ",\n"), "\n"
  , [Indent, "}"]
  ];
pp_json(Indent, [H | _] = Union) when is_list(H);
                                      is_binary(H) ->
  [ [Indent, "[\n"]
  , infix([ pp_json(Indent ++ indent(), Member)
          || Member <- Union], ",\n"), "\n"
  , [Indent, "]"]
  ].

fmt(A) when is_atom(A)   -> fmt(atom_to_list(A));
fmt(S) when is_list(S)   -> list_to_binary(["\"", S, "\""]);
fmt(B) when is_binary(B) -> B.

indent() -> lists:duplicate(?INDENT_WIDTH, $\s).

bin(IoList) -> iolist_to_binary(IoList).

infix([], _Sep)     -> [];
infix([H], _Sep)    -> [H];
infix([H | T], Sep) -> [H, Sep | infix(T, Sep)].

-spec ns(namespace(), type_id()) -> namespace().
ns(Namespace, ID) -> string:join([Namespace, id_to_name(ID)], ".").

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
def(#t{def = range, args = {Min, Max}}) ->
  case Min >= ?INT_MIN andalso Max =< ?INT_MAX of
    true  -> long;
    false -> string
  end;
def(T) ->
  throw({incompatible, T}).

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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
