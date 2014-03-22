
-module(erlsc_sample_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ t_units/1
        , t_basic_info/1
        , t_bad_union/1
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
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_Case, Config) -> Config.

end_per_testcase(_Case, _Config) -> ok.

t_units(Config) when is_list(Config) ->
  ok = erlsc_types:test(),
  ok = erlsc_sample:test(),
  ok.

t_basic_info(Config) when is_list(Config) ->
  SpecFile = filename:join(code:priv_dir(erlsc), "sample.spec"),
  Encoder = erlsc:compile({file, SpecFile}),
  Root      = {erlsc_sample, record, person},
  Data      = erlsc_sample:random_guy(term),
  ExpJson   = erlsc_sample:random_guy(json),
  {_, Json} = Encoder(Root, Data, [json]),
  ?assertEqual(ExpJson, Json).

t_bad_union(Config) when is_list(Config) ->
  Specs = [ {roots, [{erlsc_bad_sample, record, bad_sample}]}
          , {erlsc_file, "/tmp/erlsc/bad_sample.eterm"}
          , {avsc, [{output_dir, "/tmp/avsc/avsc"}]}
          ],
  ?assertException(throw, {no_transform_allowed_in_ambiguous_union, _},
                   erlsc:compile(Specs)).

%%%_* Internals ================================================================

config(Key, Config) ->
  {Key, Value} = lists:keyfind(Key, 1, Config),
  Value.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
