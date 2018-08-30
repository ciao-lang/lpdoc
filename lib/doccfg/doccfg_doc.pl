:- module(doccfg_doc, [], [doccfg]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).
:- doc(nodoc,fsyntax).
:- doc(nodoc,regtypes).

:- doc(filetype, package).

:- doc(title, "Documentation configuration options").

:- doc(module, "These are the predicates that define the options that
   can be used in documentation configuration files. Their admissible
   values are given in their corresponding types in @ref{Admissible
   values for the documentation configuration options}. See
   @ref{Writing documentation} for an introduction.").

:- doc(usage, "Use with the @tt{:- module(_, [], [doccfg])} directive
   in the documentation configuration file and provide the definition
   for the required entries.").

:- doc(hide, '$implements'/1).

% (A dummy file to document 'doccfg' as a trait)

doc_structure := '<MainFile>'.
