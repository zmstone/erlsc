
-ifndef(ERLSC_PRIVATE).
-define(ERLSC_PRIVATE, true).

-include("erlsc.hrl").

%% erlsc primitives
-type erl_primitive() ::
        arity
      | atom
      | binary
      | boolean
      | byte
      | enum %% erlsc magic
      | float
      | integer
      | iodata
      | iolist
      | module
      | neg_integer
      | nil
      | non_neg_integer
      | pos_integer
      | range
      | string.

%% partially supported avro primitives
%% http://avro.apache.org/docs/current/spec.html
-type avro_primitive() ::
        boolean
      | bytes
      | double
      | long
      | null
      | string.

-type avro_complex() ::
        array
      | enum
      | record
      | union.

-type avro_type() :: avro_primitive() | avro_complex().
-type avro_json() :: term().

%% Allowed Erlang primitives
-define(IS_PRIMITIVE(T),
        ( T =:= arity           orelse
          T =:= atom            orelse
          T =:= binary          orelse
          T =:= boolean         orelse
          T =:= byte            orelse
          T =:= enum            orelse
          T =:= float           orelse
          T =:= integer         orelse
          T =:= iodata          orelse
          T =:= iolist          orelse
          T =:= module          orelse
          T =:= neg_integer     orelse
          T =:= nil             orelse
          T =:= non_neg_integer orelse
          T =:= pos_integer     orelse
          T =:= range           orelse
          T =:= string
        )).

%% Restricted Erlang built-in types
-define(IS_RESTRICTED(T),
        ( T =:= any       orelse
          T =:= dict      orelse
          T =:= 'fun'     orelse
          T =:= gb_tree   orelse
          T =:= pid       orelse
          T =:= port      orelse
          T =:= reference orelse
          T =:= set       orelse
          T =:= term
        )).

%% Erlang list types
-define(IS_LIST(T),
        ( T =:= list          orelse
          T =:= nonempty_list orelse
          T =:= improper_list orelse
          T =:= maybe_improper_list
        )).

%% Type definition
-type def() :: record
             | list
             | nonempty_list
             | improper_list
             | maybe_improper_list
             | tuple
             | union
             | erl_primitive().

-type root_id() :: {module(), atom(), atom() | non_neg_integer()}.
-type type_id() :: root_id() | atom() | pos_integer().

-define(IS_ROOT(ID), (is_tuple(ID) andalso size(ID) =:= 3)).

%% type info in data error reason
-type type_info()   :: def() | {def(), term()}.

%% type definition extracted from eralng variable type specs
-record( t
       , { %% record field name or tuple/union element index
           id   = [] :: type_id()
           %% type definition
         , def  = [] :: def()
           %% reference to other type definition
         , ref  = [] :: root_id() | [root_id()]
           %% extra arguments of type definition, e.g. min max of a range type
         , args = [] :: any()
           %% deeper layer type definitions, valid when it's not primitive
         , subs = [] :: [type()]
         }
       ).

%% data extraction directory, built from a data path collection
-type type()     :: #t{}.     %% type info extracted from specs

-type filename() :: string().

-define(INT_MIN, -9223372036854775808).
-define(INT_MAX,  9223372036854775807).

-type namespace() :: string() | atom().
-type proplist() :: [{atom(), term()}].

-type encode_opt() :: json | binary.

-type encoder() :: fun((root_id(), term(), [encode_opt()]) ->
                          {TypeName::binary(), Data::binary()}).

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
