
-module(erlsc).

%% API
-export(
  [ compile/1
  , load/1
  ]).

-include("erlsc_private.hrl").

%%%_* Types ====================================================================

-export_type(
  [ bytes/0
  , date/0
  , int64/0
  , null/0
  , time/0
  , timestamp/0
  ]).

-type bytes()     :: [byte()].
-type date()      :: {Year::int64(), Month::int64(), Day::int64()}.
-type int64()     :: ?INT_MIN..?INT_MAX.
-type null()      :: null.
-type time()      :: {Hour::int64(), Minute::int64(), Second::int64()}.
-type timestamp() :: {Date::date(), Time::time(), MicroSec::int64()}.

%% transform types
-export(
  [ timestamp/1
  ]).
-export_type(
  [ timestamp/1
  ]).

-type timestamp(_) :: timestamp().

%%%_* APIs =====================================================================

%% @doc erlsc:timestamp/1 transform callback.
-spec timestamp(integer() | erlang:timestamp()) -> timestamp().
timestamp(GSecs) when is_integer(GSecs) ->
  {Date, Time} = calendar:gregorian_seconds_to_datetime(GSecs),
  {Date, Time, 0};
timestamp({_Mega, _Sec, Micro} = Ts) ->
  {Date, Time} = calendar:now_to_universal_time(Ts),
  {Date, Time, Micro}.

%% @doc Compile Erlang type specs to avro schema and return data serializer
%% function. Compilation inputs are either defined in a spec file or passed
%% in directly as a prop-list. See priv/sample.spec for example.
%% @end
-spec compile({file, filename()} | proplist()) -> encoder().
compile({file, SpecFile}) ->
  Specs = erlsc_specs:consult(SpecFile),
  compile(Specs);
compile(Specs) when is_list(Specs) ->
  erlsc_codec:compile(Specs).

%% @doc For the same purpose as compile/1 only the types are loaded from
%% the file specified in input spec. See priv/sample.spec for example.
%% This is to enable schema compilation in Erlang source code compile time
%% and fast load in runtime.
%% @end
-spec load({file, filename()} | proplist()) -> encoder().
load({file, SpecFile}) ->
  Specs = erlsc_specs:consult(SpecFile),
  load(Specs);
load(Specs) when is_list(Specs) ->
  erlsc_codec:load(Specs).

%%%_* Internals ================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
