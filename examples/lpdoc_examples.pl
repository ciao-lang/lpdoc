:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"An example - Documenting a library module").

:- doc(author,"Manuel Hermenegildo").

:- doc(module,"

@comment{@section{Examples} }

An example @cindex{example of lpdoc use} of the use of
@apl{lpdoc} is this manual, which can be built in the @tt{doc}
directory of the @apl{lpdoc} distribution.  Other examples of
manuals generated using @apl{lpdoc} can be found in the @apl{Ciao}
system and preprocessor @tt{doc} directories (i.e., most of the
@apl{Ciao} manuals are generated using @apl{lpdoc}) and in the 
various @apl{Ciao} bundles (typically each has an @apl{lpdoc} manual).  
Some simpler examples can be found in the @tt{examples} directory of the
@apl{lpdoc} distribution. In particular, the chapter following this
one contains the documentation generated automatically for the module
defined by file @tt{examples/example_module.pl} (which for simplicity
contains only assertions, i.e., no actual code) and which is included
in source form below. Comparing this code with the output in the
following chapter illustrates the use and some of the capabilities of
@apl{lpdoc}:

@includecode{example_module.pl}

").


