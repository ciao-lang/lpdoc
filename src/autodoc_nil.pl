:- module(autodoc_nil, [], [assertions, regtypes, fsyntax]).
% (Nothing is exported, because everything works using hooks)

:- doc(title,"Nil backend").
:- doc(author,"Miguel Ángel Sánchez").

:- doc(module,  "This module implements a backend that produces no output. This is useful,
e.g., to perform a quick syntactic check on the documentation.").

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_doctree)).

:- multifile autodoc_escape_string_hook/5.

autodoc_escape_string_hook(nil, _InputType, _NS, _DocSt, _VS) :- !.

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
    : backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(nil, DocSt, Command, NewCommand) :- !,
    rw_command(Command, DocSt, NewCommand).

% ......................................................................

rw_command(_, _, _) :- !.

% ===========================================================================

:- multifile autodoc_is_operational_hook/2.
autodoc_is_operational_hook(nil, _).

:- multifile autodoc_finish_hook/1.
autodoc_finish_hook(nil) :- true.

:- multifile autodoc_gen_alternative_hook/2.
% note: no alternative formats here
autodoc_gen_alternative_hook(nil, _) :- fail.

