:- module(_, [], [dynmod_holder]).
% (Holder for doccfg modules)

:- use_module(engine(hiord_rt), [call/1]).
:- use_module(engine(prolog_flags), [prolog_flag/3]).

:- export(call_unknown/1).
% TODO: Use interfaces?
call_unknown(G) :-
	prolog_flag(unknown, Old,  fail),
	prolog_flag(quiet,   QOld, error),
	( call(G),
	  prolog_flag(unknown, _, Old),
	  prolog_flag(quiet, _, QOld)
	; prolog_flag(unknown, _, Old),
	  prolog_flag(quiet, _, QOld),
	  fail
	).

