
%% extract type definitions out of a given module
%% and compile into a nicer format

-module(erlsc_types).

-export([ compile/1
        , consult_file/1
        , is_instance/2
        , is_null/1
        , scan/2
        , write_to_file/2
        ]).

-include("erlsc_private.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% record type id
-define(REC_ID(Module, RecordName), {Module, record, RecordName}).

%% reference type id
-define(REF_ID(Module, Type, Arity), {Module, Type, Arity}).

-type form() :: erl_parse:abstract_form().
-type location() :: {module(), Line::pos_integer() | {record, atom()}}.

%% @doc Compile a module, all type definitions in a module are extracted to
%% a nicer format. if non-supported type is found, an {nok, Reason} value
%% is set to #t.def.
%% @end
-spec compile(module()) ->  [type()].
compile(Module) when is_atom(Module) ->
  BeamFile = beam_file(Module),
  compile_forms(Module, module_types(BeamFile)).

-spec consult_file(filename()) -> [type()] | no_return().
consult_file(Filename) ->
  {ok, Proplists} = file:consult(Filename),
  [from_proplist(Props) || Props <- Proplists].

-spec write_to_file(filename(), [type()]) -> ok | no_return().
write_to_file(Filename, Types) ->
  file:write_file(Filename,
    lists:map(fun(Type) ->
                io_lib:format("~p.\n", [to_proplist(Type)])
              end, Types)).

%% @doc Shallow test to check if the given data is an instance of given type.
-spec is_instance(term(), type()) -> true | type_info().
is_instance(X, #t{ id   = {Module, record, Tag}
                 , def  = record
                 , subs = SubTypes
                 }) ->
  ( is_tuple(X) andalso
    element(1, X) =:= Tag andalso
    size(X) - 1 =:= length(SubTypes)
  ) orelse {Module, record, Tag};
is_instance(X, #t{ def  = tuple
                 , subs = SubTypes
                 }) ->
  ( is_tuple(X) andalso
    size(X) =:= length(SubTypes)
  ) orelse {tuple, length(SubTypes)};
is_instance(X, #t{def = range, args = {Min, Max}}) ->
  ( X >= Min andalso X =< Max
  ) orelse {range, {Min, Max}};
is_instance(X, #t{def = enum, args = Symbols}) ->
  ( is_atom(X) andalso lists:member(X, Symbols)
  ) orelse {enum, Symbols};
is_instance(X, #t{def = atom, args = Args}) ->
  case Args of
    []  -> is_atom(X) orelse atom;
    Val -> is_null(Args, X) orelse X =:= Val orelse {atom, Val}
  end;
is_instance(X, #t{def = integer, args = Args}) ->
  case Args of
    []  -> is_integer(X) orelse integer;
    Val -> X =:= Val orelse {integer, Val}
  end;
is_instance(X, #t{ def = Def }) ->
  is_inst(X, Def) orelse Def.

-spec is_null(atom(), atom()) -> boolean().
is_null(InSpec, RealData) ->
  is_null(InSpec) andalso is_null(RealData).

%% @doc Check if it's a null instance
-spec is_null(atom()) -> boolean().
is_null(nil)       -> true;
is_null(null)      -> true;
is_null(undefined) -> true;
is_null([])        -> true;
is_null(<<>>)      -> true;
is_null(_)         -> false.

%% @doc Scan erlang source code to get type definition.
-spec scan(pos_integer(), string()) -> type().
scan(Line, String) ->
  {ok, Tokens, _} = erl_scan:string(String, Line),
  {ok, Form0} = erl_parse:parse_form(Tokens),
  Form =
    case Form0 of
      {attribute, Line, record, {Name, Fields}} ->
        {attribute, Line, type, {{record, Name}, Fields}};
      _ ->
        Form0
    end,
  RawTypes = filter_type_spec_forms([Form]),
  [Type] = compile_forms(?MODULE, RawTypes),
  Type.

%%%_* Internals ================================================================

%% @private Compile abstract code forms.
-spec compile_forms(module(), [form()]) -> [type()].
compile_forms(Module, Forms) ->
  [root(Module, Form) || Form <- Forms].

-spec is_inst(term(), def()) -> boolean().
is_inst(X, nonempty_list) ->
  case X of
    [_|_] -> true;
    _     -> false
  end;
is_inst(X, arity)           -> X >= 0 andalso X =< 255;
is_inst(X, binary)          -> is_binary(X);
is_inst(X, boolean)         -> is_boolean(X);
is_inst(X, byte)            -> X >= 0 andalso X =< 255;
is_inst(X, float)           -> is_float(X);
is_inst(X, iodata)          -> is_binary(X) orelse is_list(X);
is_inst(X, iolist)          -> is_list(X);
is_inst(X, list)            -> is_list(X);
is_inst(X, module)          -> is_atom(X);
is_inst(X, neg_integer)     -> is_integer(X) andalso X < 0;
is_inst(X, non_neg_integer) -> is_integer(X) andalso X >= 0;
is_inst(X, nil)             -> X =:= [];
is_inst(X, pos_integer)     -> is_integer(X) andalso X > 0;
is_inst(X, string)          -> is_list(X);
is_inst(_, union)           -> true. %% union, tested in deeper layer

%% root of type definitions in a module, either type redefine or record
-spec root(module(), any()) ->  type().
root(Module, {{record, Name}, Fields, _}) ->
  root(Module, {{record, Name}, Fields});
root(Module, {{record, Name}, Fields}) ->
  RootId = ?REC_ID(Module, Name),
  Fun = fun() ->
          Subs = [rec_field_type(Module, F) || F <- Fields],
          #t{ id   = RootId
            , def  = record
            , subs = verify_sub_names({Module, {record, Name}}, Subs)
            }
        end,
  try_root(RootId, Fun);
root(Module, {Name, TypeDef, Args}) ->
  RootId_safe = ?REF_ID(Module, Name, length(Args)),
  Fun = fun() ->
          Line = element(2, TypeDef),
          RootId = redef_id({Module, Line}, Module, Name, Args),
          T = tree(Module, TypeDef),
          T#t{id = RootId}
        end,
  try_root(RootId_safe, Fun).

try_root(RootId, Fun) ->
  try Fun()
  catch throw : Reason -> #t{id = RootId, def = {error, Reason}}
  end.

%% type definition tree
tree(Mod, {type, Line, T, _}) when ?IS_RESTRICTED(T) ->
  excep({Mod, Line}, "type ~w is not supported", [T]);
tree(Mod, {var, Line, '_'}) ->
  excep({Mod, Line}, "'any()' or '_' type is not supported");
tree(Mod, {var, Line, _VarName}) ->
  excep({Mod, Line}, "variable type is not supported");
tree(Mod, {paren_type, _Line, [Type]}) ->
  tree(Mod, Type);
tree(Mod, {ann_type, _Line1, [{var, _Line2, Name}, Type]}) ->
  T = tree(Mod, Type),
  T#t{id = Name};
tree(_Mod, {atom, _Line, Value}) ->
  #t{ def  = atom
    , args = Value
    };
tree(_Mod, {integer, _Line, Value}) ->
  #t{ def  = integer
    , args = Value
    };
tree(Mod, {op, _Line, '-', Int}) ->
  T = tree(Mod, Int),
  T#t{args = -T#t.args};
tree(Mod, {op, _Line, '+', Int}) ->
  tree(Mod, Int);
tree(Mod, {type, Line, T, Args}) when ?IS_PRIMITIVE(T) ->
  #t{ def  = T
    , args = args(Mod, Line, T, Args)
    };
tree(Mod, {type, _Line, T, [ListOfWhat]}) when ?IS_LIST(T) ->
  Sub = tree(Mod, ListOfWhat),
  #t{ def  = T
    , subs = [set_id(Sub, 1)]
    };
tree(Mod, {type, Line, T, Elements}) when T =:= tuple orelse T =:= union ->
  %% TODO do not allow union of two or more lists
  TaggedElements = lists:zip(lists:seq(1, length(Elements)), Elements),
  Subs = [ set_id(tree(Mod, E), I) || {I, E} <- TaggedElements ],
  #t{ def  = T
    , subs = verify_sub_names({Mod, Line}, Subs)
    };
tree(_Mod, {type, _Line, module, _}) ->
  #t{ def  = atom
    };
tree(Mod, {type, _Line, record, RecName}) ->
  [{atom, _, Name}] = RecName,
  #t{ref = ?REC_ID(Mod, Name)};
tree(Mod, {remote_type, Line, MTA}) ->
  [{atom, _, M}, {atom, _, T}, Args] = MTA,
  #t{ref = redef_id({Mod, Line}, M, T, Args)};
tree(Mod, {type, Line, Type, Args}) ->
  %% redefine of local type
  #t{ref = redef_id({Mod, Line}, Mod, Type, Args)}.

-spec verify_sub_names(location(), [type()]) -> [type()] | no_return().
verify_sub_names(Location, Subs) ->
  Names = [ID || #t{id = ID} <- Subs],
  assert_no_sub_name_duplication(Location, lists:sort(Names)),
  Subs.

assert_no_sub_name_duplication(_Location, [_]) -> ok;
assert_no_sub_name_duplication(Location, [N, N | _]) ->
  excep(Location, "duplicated sub names ~p", [N]);
assert_no_sub_name_duplication(Location, [_ | Rest]) ->
  assert_no_sub_name_duplication(Location, Rest).

%% @private Set ID if it's not set already.
-spec set_id(type(), type_id()) -> type().
set_id(#t{id = []} = T, Id) -> T#t{id = Id};
set_id(Type, _Id)           -> Type.

redef_id(Location, _M, T, Args) when length(Args) > 1 ->
  excep(Location, "bad arity in type spec ~p", [T]);
redef_id(_Location, M, T, Args) ->
  ?REF_ID(M, T, length(Args)).

excep(Location, MsgFmt, Args) ->
  excep("~p: " ++ MsgFmt, [Location | Args]).

excep(MsgFmtStr, Args) when is_list(MsgFmtStr) ->
  erlang:throw(lists:flatten(io_lib:format(MsgFmtStr, Args)));
excep(Location, Msg) when is_tuple(Location) ->
  excep(Location, Msg, []).

%% @private Get the beam file name of the given module.
-spec beam_file(module()) -> filename().
beam_file(Module) ->
  case code:which(Module) of
    non_existing -> throw({"module not found", Module});
    File         -> File
  end.

%% @private Get all the type definitions out of abstract code.
module_types(BeamFile) ->
  {ok, AbstractCode} = beam_lib:chunks(BeamFile, [abstract_code]),
  {_Module,[{abstract_code,{raw_abstract_v1, Forms}}]} = AbstractCode,
  filter_type_spec_forms(Forms).

%% @private Get type specs from code form.
-spec filter_type_spec_forms([form()]) -> [form()].
filter_type_spec_forms(Forms) ->
  [ Type || {attribute, _Line, Tag, Type} <- Forms
  , Tag == type orelse Tag == opaque ].

%% @private Get record field type.
rec_field_type(Module, {record_field, Line, _Name}) ->
  excep({Module, Line}, "missing spec");
rec_field_type(Module, {record_field, Line, _Name, _Value}) ->
  excep({Module, Line}, "missing spec");
rec_field_type(Module, {typed_record_field, Name, Type}) ->
  T0 = tree(Module, Type),
  %NewID = case T0#t.id of
  %          [] -> rec_field_name(Name);
  %          Id -> Id
  %        end,
  NewID = rec_field_name(Name),
  T = T0#t{id = NewID},
  %% So Erlang compiler is smart: it adds an atom 'undefined' type as
  %% a union branch if there is no default value set for the record
  %% field definition. e.g. #record{field :: integer()} is actually
  %% interpreted as #record{field :: undefiend | integer()}
  %% here we remove this auto-added 'undefined' union branch.
  case T of
    #t{def = union, subs = Branches} ->
      case Branches of
        [#t{def = atom, args = undefined}, Another] ->
          Another#t{id = NewID};
        [#t{def = atom, args = undefined} | Subs] ->
          Tmp = lists:zip(lists:seq(1, length(Subs)), Subs),
          NewSubs = [ case is_atom(S#t.id) of
                        true  -> S;
                        false -> S#t{id = I}
                      end || {I, S} <- Tmp
                    ],
          T#t{subs = NewSubs};
        Branches ->
          Branches %% no difference
      end;
    _Other ->
      T
  end.

%% @private Get record field name.
rec_field_name({record_field,_,{atom,_,N}})   -> N;
rec_field_name({record_field,_,{atom,_,N},_}) -> N.

%% @private Get extra arguments for type definitions.
args(_Mod, _Line, range, Range) ->
  [Min, Max] = Range,
  {arg_int(Min), arg_int(Max)};
args(Mod, Line, enum, UnionRaw) ->
  length(UnionRaw) > 1 andalso excep({Mod, Line}, "bad arity in enum"),
  case enum_elements(tree(Mod, hd(UnionRaw))) of
    {ok, Elements}  -> Elements;
    {error, Reason} -> excep({Mod, Line}, "~p", [Reason])
  end;
args(_Mod, _Line, _, _) ->
  [].

%% @private Get integer value out of range spec definition.
-spec arg_int(form()) -> integer().
arg_int({integer, _Line, I})           -> I;
arg_int({op, _, '-', {integer, _, I}}) -> -I.

%% @private Get enum type elements.
-spec enum_elements(type()) -> {ok, [atom()]} | {error, any()}.
enum_elements(#t{def = union, subs = Elements}) ->
  Fun = fun(#t{def = atom, args = Symbol}) when is_atom(Symbol) -> Symbol;
           (Type)                                               -> Type
        end,
  Enums = lists:map(Fun, Elements),
  case [T || #t{} = T <- Enums] of
    []    -> {ok, Enums};
    Unexp -> {error, {"unexpected types", Unexp}}
  end;
enum_elements(Type) ->
  {error, {"unexpected type", Type}}.

%% @private Build #t{} from a proplist.
-spec from_proplist({root_id(), proplist()}) -> [type()].
from_proplist({ID, Props}) ->
  Find = fun(Key) ->
           case lists:keyfind(Key, 1, Props) of
             false      -> [];
             {Key, Val} -> Val
           end
         end,
  #t{ id   = ID
    , def  = Find(def)
    , ref  = Find(ref)
    , args = Find(args)
    , path = Find(path)
    , subs = [from_proplist(KVL) || KVL <- Find(subs)]
    }.

%% @private Convert #t{} to a proplist.
-spec to_proplist(type()) -> {type_id(), proplist()}.
to_proplist(#t{ id   = ID
              , def  = Def
              , ref  = Ref
              , args = Args
              , path = Path
              , subs = Subs
              }) ->
  Props =
    [{def, Def}   || Def  =/= []] ++
    [{ref, Ref}   || Ref  =/= []] ++
    [{args, Args} || Args =/= []] ++
    [{path, Path} || Path =/= []] ++
    [{subs, [to_proplist(Sub) || Sub <- Subs]} || Subs =/= []],
  {ID, Props}.

%%% Tests ======================================================================

-ifdef(EUNIT).

-define(undef, undefined).

-define(MATCH(PAT, EXP), ?assertMatch(PAT, EXP)).
-define(EQ(VAL, EXPR), ?assertEqual(VAL, EXPR)).
-define(SAMPLE, erlsc_sample).

find(Module, RootId) ->
  Types = compile(Module),
  case lists:keyfind(RootId, #t.id, Types) of
    #t{def = {error, Reason}} ->
      excep({?MODULE, ?LINE}, Reason);
    Type ->
      Type
  end.

-define(SCAN(STR), scan(?LINE, STR)).
-define(assertSubstr(Str, Sub), assertSubstr(?LINE, Str, Sub)).

assertSubstr(Line, String, SubStr) ->
  case string:str(String, SubStr) > 0 of
    true  -> ok;
    false -> throw({Line, "assertSubstr failed", String, SubStr})
  end.

%% make a type definition
t(ID, Def) -> t(ID, Def, [], []).

t(ID, Def, Subs) -> t(ID, Def, Subs, []).

t(ID, Def, Subs, Args) -> #t{ id   = ID
                            , def  = Def
                            , subs = Subs
                            , args = Args
                            }.
%% make a refreence to another type definition
r(ID, Ref) -> #t{ id  = ID
                , ref = Ref
                }.

range_type_test() ->
  Root = {erlsc, int64, 0},
  ?EQ(t(Root, range, [], {?INT_MIN, ?INT_MAX}), find(erlsc, Root)).

forward_reference_test() ->
  Root = {?SAMPLE, person, 0},
  ?EQ(r(Root, {?SAMPLE, record, person}), find(?SAMPLE, Root)).

tuple_test() ->
  Root = {?SAMPLE, name, 0},
  TupleElements = [ t('FirstName',  string)
                  , t('MiddleName', union, [ r(1,{erlsc_sample, null, 0})
                                           , t(2,string)
                                           ])
                  , t('LastName',   string)],
  Type = find(?SAMPLE, Root),
  %%ct:pal("~p\n", [TupleElements]),
  %%ct:pal("~p\n", [Type]),
  ?EQ(t(Root, tuple, TupleElements), Type).

%% restricted types should end up with def = {error, "...is not supported..."}
restricted_erl_primitives_test() ->
  Code = fun(Spec) -> "-type x() :: " ++ Spec ++ "." end,
  Types =
    [ ?SCAN(Code("any()"))
    , ?SCAN(Code("dict()"))
    , ?SCAN(Code("fun(()->ok)"))
    , ?SCAN(Code("gb_tree()"))
    , ?SCAN(Code("pid()"))
    , ?SCAN(Code("port()"))
    , ?SCAN(Code("reference()"))
    , ?SCAN(Code("term()"))
    , ?SCAN(Code("{integer(), A}"))
    ],
  Verify = fun(#t{def = {error, Reason}}) ->
             ?assertSubstr(Reason, "is not supported")
           end,
  lists:foreach(Verify, Types).

%% unsupported type arity
bad_arity_test() ->
  #t{def = {error, Reason1}} =
    ?SCAN("-type x(_, _) :: string()."),
  #t{def = {error, Reason2}} =
    ?SCAN("-type x() :: {nil, mod:type(atom(),integer())}."),
  #t{def = {error, Reason3}} =
    ?SCAN("-type x() :: enum(x, y)."),
  ?assertSubstr(Reason1, "bad arity in type spec"),
  ?assertSubstr(Reason2, "bad arity in type spec"),
  ?assertSubstr(Reason3, "bad arity in enum").

%% def = {error, "...duplicated sub names..."}
%% if two tuple elements are named the same.
duplicated_sub_names_test() ->
  Tuple = ?SCAN("-type x() :: {A::integer(), B::string(), A::nil}."),
  #t{def = {error, Reason}} = Tuple,
  ?assertSubstr(Reason, "duplicated sub names 'A'").

%% erlang compiler add a 'undefined' union branch to record fields
%% if there is no default value assigned, it should be discarded
discard_auto_added_record_field_union_branch_test() ->
  Result1 = t({?MODULE, record, x}, record, [t(a, integer)]),
  Type1 = ?SCAN("-record(x, {a :: integer()})."),
  Type2 = ?SCAN("-record(x, {a = nil :: integer()})."),
  ?EQ(Result1, Type1),
  ?EQ(Result1, Type2),
  Type3 = ?SCAN("-record(x, {a :: atom() | integer()})."),
  Result2 = t({?MODULE, record, x}, record,
              [t(a, union, [ t(1, atom)
                           , t(2, integer)
                           ])]),

  ?EQ(Result2, Type3),
  #t{def = {error, Reason1}} = ?SCAN("-record(x, {a})."),
  #t{def = {error, Reason2}} = ?SCAN("-record(x, {a = nil})."),
  ?assertSubstr(Reason1, "missing spec"),
  ?assertSubstr(Reason2, "missing spec").

%% #person{} record in erlsc_sample.erl
record_type_test() ->
  Root = {?SAMPLE, record, person},
  SnsLink_KVL =
    {'SnsLink', [ {def,  tuple}
                , {subs, [ {'Site', [{def, string}]}
                         , {'Link', [{ref, {?SAMPLE, uri, 0}}]}
                         ]}
                ]},
  Circles_KVL =
    [ {def,  union}
    , {subs, [ {1, [{ref, {?SAMPLE, null, 0}}]}
             , {2, [ {def,  list}
                   , {subs, [{1, [{ref, {?SAMPLE, record, circle}}]}]}
                   ]}
             ]}
    ],
  KVL =
    { Root
    , [ {def,  record}
      , {subs, [ {id,       [{ref, {?SAMPLE, person_id, 0}}]}
               , {name,     [{ref, {?SAMPLE, name, 0}}]}
               , {gender,   [{ref, {?SAMPLE, gender, 0}}]}
               , {birthday, [{ref, {erlsc, date, 0}}]}
               , {email, [ {def,  union}
                         , {subs, [ {1, [{ref, {?SAMPLE, null, 0}}]}
                                  , {2, [{def, string}]}
                                  ]}
                         ]}
               , {sns_links, [ {def,  list}
                             , {subs, [SnsLink_KVL]}
                             ]}
               , {addresses, [ {def,  list}
                             , {subs, [{1, [{ref, {?SAMPLE, address, 0}}]}]}
                             ]}
               , {phone_nums, [ {def,  list}
                              , {subs, [{1, [{ref, {?SAMPLE, phone_num, 0}}]}]}
                              ]}
               , {circles, Circles_KVL}
               , {terms, [{ref, {?SAMPLE, term_string, 1}}]}
               ]}
      ]
    },
  Expected = from_proplist(KVL),
  %io:format("~p\n", [Expected]),
  Value = find(?SAMPLE, Root),
  %io:format("~p\n", [Value]),
  ?EQ(Expected, Value).

-endif. %%(EUNIT).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
