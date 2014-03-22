
-module(erlsc_sample_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ t_units/1
        , t_basic_info/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("erlsc.hrl").

all() ->
  [F || {F, _} <- module_info(exports)
      , case atom_to_list(F) of
          "t_" ++ _ -> true;
          _         -> false
        end].

init_per_suite(Config) ->
  SpecFile = filename:join(code:priv_dir(erlsc), "sample.spec"),
  Encoder = erlsc:compile({file, SpecFile}),
  [{encoder, Encoder} | Config].

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) -> Config.

end_per_testcase(_Case, _Config) -> ok.

t_units(Config) when is_list(Config) ->
  ok = erlsc_types:test(),
  ok = erlsc_sample:test(),
  ok.

t_basic_info(Config) when is_list(Config) ->
  Encoder   = config(encoder, Config),
  Root      = {erlsc_sample, record, person},
  Data      = erlsc_sample:random_guy(term),
  ExpJson   = erlsc_sample:random_guy(json),
  {_, Json} = Encoder(Root, Data, [json]),
  ?assertEqual(ExpJson, Json).

%%%_* Internals ================================================================

config(Key, Config) ->
  {Key, Value} = lists:keyfind(Key, 1, Config),
  Value.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
