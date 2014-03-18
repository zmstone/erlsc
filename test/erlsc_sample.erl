
%% sample types

-module(erlsc_sample).

%% transform callbacks
-export(
  [ term_string/1
  ]).

-export(
  [ random_guy/1
  ]).

-export_type(
  [ person/0
  , term_string/1
  ]).

-include("../src/erlsc_private.hrl").
-include_lib("eunit/include/eunit.hrl").

-type gender()       :: enum(female | male).
-type uri()          :: string().
-type phone_num()    :: PhoneNum::{Tag::string() | atom(),
                                   Num::string() | integer()}.
-type null()         :: erlsc:null().
-type country_code() :: 1..999.
-type person_id()    :: erlsc:int64().

-type name() :: { FirstName::string()
                , MiddleName::null()|string()
                , LastName::string()
                }.

-record(address,
        { tags    :: [AddrTag::enum(residence | shipping | billing)]
        , street  :: string()
        , city    :: string()
        , country :: country_code()
        , zip     :: null() | erlsc:int64()
        }).
-type address() :: #address{}.

-record(circle,
        { name :: string()
        , ids  :: [person_id()]
        }).

-record(person,
        { id         :: person_id()
        , name       :: name()
        , gender     :: gender()
        , birthday   :: erlsc:date() | null
        , email      :: null() | string()
        , sns_links  :: [SnsLink::{Site::string(), Link::uri()}]
        , addresses  :: [address()]
        , phone_nums :: [phone_num()]
        , circles    :: null() | [#circle{}]
        , terms      :: term_string(term())
        }).
-type person() :: #person{}.

-type term_string(_) :: string().
-spec term_string(term()) -> string().
term_string(Term) -> lists:flatten(io_lib:format("~w", [Term])).

%%%_* APIs =====================================================================

random_guy(term) ->
  Addr1 = #address{ tags    = [residence, billing]
                  , street  = "street 423"
                  , city    = "stockholm"
                  , country = 46
                  , zip     = 16356
                  },
  Addr2 = #address{ tags    = [shipping]
                  , street  = "street 424"
                  , city    = "stockholm"
                  , country = 46
                  , zip     = undefined
                  },
  #person{ id         = 42
         , name       = {"random", null, "guy"}
         , gender     = male
         , birthday   = {1970, 1, 1}
         , email      = "email@some.com"
         , sns_links  = []
         , addresses  = [Addr1, Addr2]
         , phone_nums = [{home, 123455}]
         , circles    = nil
         , terms      = [whatever]
         };
random_guy(json) ->
  "".

%%%_* Internals ================================================================

%%% Tests ======================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
