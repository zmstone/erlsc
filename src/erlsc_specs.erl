
-module(erlsc_specs).

-export([ consult/1
        , get_avsc_specs/1
        , get_erlsc_file/1
        , get_roots/1
        ]).

-export_type([ specs/0
             ]).

-include("erlsc_private.hrl").

-type spec() :: {atom(), term()}.
-type specs() :: [spec()].

-spec consult(filename()) -> specs().
consult(Filename) ->
  {ok, Specs} = file:consult(Filename),
  Specs.

-spec get_roots(specs()) -> [root_id()].
get_roots(Specs) ->
  keyfind(roots, Specs).

get_avsc_specs(Specs) ->
  keyfind(avsc, Specs).

get_erlsc_file(Specs) ->
  keyfind(erlsc_file, Specs).

keyfind(Key, KVL) ->
  case lists:keyfind(Key, 1, KVL) of
    {_Key, Val} -> Val;
    false       -> throw({not_found, {Key, KVL}})
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
