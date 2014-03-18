
-module(erlsc_sup).

-behaviour(supervisor).

-export([ init/1
        , start_link/0
        ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 10, 5}, []}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
