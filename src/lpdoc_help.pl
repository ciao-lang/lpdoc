:- module(lpdoc_help, [], [assertions, dcg, fsyntax]).

:- doc(title, "Open help on module or predicates").

:- doc(module, "This module opens the documentation for a module or
   predicate. Example:

@begin{verbatim}
?- use_module(lpdoc(lpdoc_help)).
?- help(library(lists)).
?- help(library(lists), append/3).
@end{verbatim}
").

:- doc(bug, "Optimize such that lpdoc_single_mod can be incremental").
:- doc(bug, "Allow @tt{build/site} URLs").

:- use_module(library(opendoc)).
:- use_module(lpdoc(lpdoc_single_mod), [
    lpdoc_single_mod/1,
    ext_html_doc_file/2,
    add_pred_anchor_url/4]).

:- export(help/2).
:- pred help(+Spec, +Pred)
   # "Opens the generated (HTML) documentation of predicate @var{Pred}
      (@tt{Name/Arity}) of module @var{Spec} (using @lib{opendoc})".

help(Spec, Name/Arity) :-
    ( lpdoc_single_mod(Spec) ->
        true
    ; throw(error(no_doc_for(Spec), help/2)) % TODO: message?
    ),
    DocPath = ~ext_html_doc_file(Spec),
    PredDoc = ~add_pred_anchor_url(DocPath, Name, Arity),
    opendoc(~file_url(PredDoc)).

% Obtain a file:// URL for an absolute file path
file_url(AbsF) := ~atom_concat('file://', AbsF).

:- export(help/1).
:- pred help(Spec) # "Opens the generated (HTML) documentation of
   module @var{Spec} (using @lib{opendoc})".

help(Spec) :-
    ( lpdoc_single_mod(Spec) ->
        true
    ; throw(error(no_doc_for(Spec), help/1)) % TODO: message?
    ),
    DocPath = ~ext_html_doc_file(Spec),
    opendoc(~file_url(DocPath)).
