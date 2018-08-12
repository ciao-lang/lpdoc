:- module(doccfg_doc, [], [doccfg]).

:- doc(title, "Documentation configuration options").

:- doc(usage, "Use with the @tt{:- module(_, [], [doccfg])} directive
   in the documentation configuration file and provide the definition
   for the required entries.").

:- doc(hide, '$implements'/1).

% (A dummy file to document 'doccfg' as a trait)

doc_structure := '<MainFile>'.
