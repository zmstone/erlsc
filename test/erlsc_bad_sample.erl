
-module(erlsc_bad_sample).

-export_type([ a/0
             , b/0
             ]).

-type a() :: {b(), integer()}.
-type b() :: a() | atom().

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
