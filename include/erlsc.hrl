
-ifndef(_ERLSC_HRL_).
-define(_ERLSC_HRL_, true).

%% a workaround to support enum type
%% enum/1 takes a union as input and consider the union as enums
%% union members must be atoms. e.g.
%% -type myenum() :: enum(a | b). is ok
%% -type myenum() :: enum(atom()). is not ok
-type enum(Union) :: Union.

-endif.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
