
-module(erlsc_bad_sample).

-type transform(_) :: string().
-type bad_union() :: transform(term()) | integer().

-record(bad_sample, {field :: bad_union()}).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
