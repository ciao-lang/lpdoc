
:- use_package([assertions]).

% :- doc(filetype, documentation).
:- doc(filetype, part).

:- doc(title,"Writing documentation").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- doc(module,"

This section includes some details for writing proper documentation in
@apl{lpdoc}, improving the layout of manuals, @concept{usage tips},
and @concept{troubleshooting} advice.

@section{Documenting source files} 

While @apl{lpdoc} can produce useful documentation from the interface
of the source file, the quality of the documentation generated can be
greatly enhanced by including within the program text:

@begin{itemize}

@item @em{assertions}, and 

@item @em{machine-readable comments}. 

@end{itemize}

@bf{Assertions} are declarations which are included in the source
program and provide the compiler with information regarding
properties of the code. Typical assertions include type
declarations, modes, computational properties (such as nonfailure or determinacy),
many compiler directives (such as @decl{dynamic/1}, @decl{op/3},
@decl{meta_predicate/1}...), etc.  When documenting a module,
@apl{lpdoc} will use the assertions associated with the module
interface to construct a textual description of this interface.  In
principle, only the exported predicates are documented, although any
predicate can be included in the documentation by explicitly
requesting it (see the documentation for the @decl{doc/2}
declaration).  Judicious use of these assertions allows at the same
time documenting the program code, documenting the external use of the
module, and greatly improving program debugging and allowing program verification.  
The latter is possible because the assertions provide the compiler
with information on the intended meaning or behaviour of the program
(i.e., the specification) which can be checked at compile-time (by a
suitable preprocessor/static analyzer) and/or at run-time (via checks
inserted by a preprocessor). See @ref{The Ciao assertion language} for
more details.

@bf{Machine-readable comments} are also declarations included in the
source program which contain additional information intended to be
read by humans (i.e., this is an instantiation of the @index{literate
programming} style of Knuth @cite{knuth-lit}). Typical such comments
include title, author(s), summary, bugs, changelog, etc.  Judicious
use of these comments allows enhancing at the same time the
documentation of the program text and the manuals generated from
it. See @ref{Documentation mark-up language and doc declarations} for
more details.  These declarations can also be writtten in wiki 
(@index{mark-down}) style -- see @lib{doccomments}.

@apl{lpdoc} requires these @bf{assertions} and @bf{machine-readable}
comments to be written using the @apl{Ciao} @em{assertion language}.
While @apl{Ciao} has core support for this language, it is however
quite straightforward in most Prolog and (C)LP systems to define a
library with dummy declarations so that the assertions and comments
meant for @apl{lpdoc} are simply ignored by the compiler, making it
possible to compile programs documented using assertions and comments
in such systems and at the same time generate documentation using
@apl{lpdoc} (as well as making other uses of the assertions such as
checking tehm statically and/or dynamically). 

@comment{It is also possible to
compiling @apl{lpdoc} itself under (C)LP systems other than the
@apl{Ciao} under which @apl{lpdoc} is developed (this has has been
done already for a number of Prolog and (C)LP systems).}

@comment{A simple compatibility library is available in order to make
it possible to compile programs documented using assertions and
comments in traditional (constraint) logic programming systems which
lack native support for them (see the @tt{compatibility} directory in
the @apl{lpdoc} library).}


@section{More complex manuals} 

Writing and generating more complex manuals involves writing a
@index{documentation configuration} (@lib{doccfg}) file (e.g,
@tt{SETTINGS.pl}) with this basic structure:

@begin{verbatim}
:- module(_, [], [doccfg]).

doc_structure := '<MAIN MODULE>'.
@end{verbatim}

The first line indicates that this is a module implementing a
@lib{doccfg}. You must edit the file to suit your needs. It is
recommended that you review at least the following points:

@begin{itemize}

@item Set the @tt{filepath} option (see @pred{filepath/1}) to include
  all additional directories where the files to be documented can be
  found (@concept{alias path}s for libraries are included
  automatically).

@item Set @tt{doc_structure} (see @pred{doc_structure/1}) to be the
  @index{document structure}.

@end{itemize}

Any option not directly specified will use the default values
indicated in @lib{doccfg}. You may however want to change several of
these:

@begin{itemize}

@item @tt{doc_mainopts}: control what is included in the documentation
  for the main file (see @pred{doc_mainopts/1}).

@item @tt{doc_compopts}: sets options for the component files (see
  @pred{doc_docopts/1}).

@item @tt{docformat}: determines the set of formats (@tt{pdf},
  @tt{ascii}, @tt{html}, @tt{info}, @tt{manl}, ...) in which the
  documentation should be generated by default (see
  @pred{docformat/1}).

@item @tt{output_name}: determines the base file name of the main
  documents generated by @apl{lpdoc} (see @pred{output_name/1}).

@item @tt{index}: determines the list of indices to be included at
  the end of the document (see @pred{index/1}).

@item @tt{bibfile}: determines a list of @index{.bib files} containing
  @index{bibliographic entries} for @concept{using citations} (see
  @pred{bibfile/1}).

@item @tt{startpage}: allows changing the page number of the first
  page of the manual (see @pred{startpage/1}).

@item @tt{papertype}: allows select several paper sizes for the
  printable outputs (@tt{pdf}) (see @pred{papertype/1}).

@end{itemize}

See @lib{doccfg} for other options.

@section{Advanced usage tips}
@cindex{usage tips}

This section contains additional suggestions on the use of
@apl{lpdoc}.

@subsection{Documenting libraries and/or applications}

As mentioned before, for each a @tt{.pl} file, @apl{lpdoc} tries to
determine whether it is a library or the main file of an application,
and documents it accordingly. Any combination of libraries and/or main
files of applications can be used arbitrarily as components or main
files of a @apl{lpdoc} manual. Some typical combinations are:

@begin{itemize}

@item @em{Main file is a library, no components:} A manual of a simple
library, which appears externally as a single module. The manual
describes the purpose of the library and its interface.

@item @em{Main file is an application, no components:} A manual of a
simple application.

@item @em{Main file is a library, components are also libraries:} This
can be used for example for generating an @concept{internals manual}
of a library. The main file describes the purpose and use of the
library, while the components describe the internal modules of the
library.

@item @em{Main file is an application, components are libraries:} This
can be used similarly for generating an @concept{internals manual} of
an application. The main file describes the purpose and use of the
application, while the components describe the internal modules which
compose the application.

@item @em{Main file is a (pseudo-)application, components are
libraries:} A manual of a complex library made up of smaller libraries
(for example, the @apl{Prolog} library). The (pseudo-)application file
contains the introductory material (title, version, etc.).  Each
chapter describes a particular library.

@item @em{Main file is a (pseudo-)application, components are
applications:} This can be used to generate a manual of a set of
applications (e.g., a set of utilities). The (pseudo-)application file
contains the introductory material (title, version, etc.).  Each
chapter describes a particular component application.

@end{itemize}

@subsection{Documenting files which are not modules}

Sometimes it is difficult for @apl{lpdoc} to distinguish
@concept{include files} and Ciao @concept{packages} from normal
@em{user} files (i.e., normal code files but which are not
modules). The distinction is important because the former are quite
different in their form of use (they are loaded via @decl{include/1}
or @decl{use_package/1} declarations instead of
@decl{ensure_loaded/1}) and effect (since they are included, they
'export' operators, declarations, etc.), and should typically be
documented differently.  There is a special @decl{doc/2}
declaration (@tt{:- doc(filetype,...).}) which provides a way of
defining the intended use of the file. This declaration is normally
not needed in modules, include files, or packages, but should be added
in user files (i.e., those meant to be loaded using
@decl{ensure_loaded/1}). Adding this declaration will, for example,
avoid spurious documentation of the declarations in the
@lib{assertions} package themselves when this package is included in a
user file.

@subsection{Splitting large documents into parts}

As mentioned before, in @apl{lpdoc} each documented file (each
component) corresponds to a chapter in the generated manual. In large
documents, it is sometimes convenient to build a super-structure of
parts, each of which groups several chapters. There is a special value
of the second argument of the @tt{:- doc(filetype,...).}  declaration
mentioned above designed for this purpose. The special @em{filetype}
value @tt{part} can be used to flag that the file in which it appears
should be documented as the start of one of the major @index{parts in
a large document}. In order to introduce such a part, a @tt{.pl} file
with a declaration @tt{:- doc(filetype,part).}  should be inserted in
the sequence of files that make up the @tt{components} variable of the
@index{documentation configuration} file at each point in which a
major part starts. The @tt{:- doc(title,""..."").}  declaration of
this file will be used as the part title, and the @tt{:-
doc(module,""..."").}  declaration text will be used as the
introduction to the part.

@subsection{Documenting reexported predicates}

Reexported predicates, i.e., predicates which are exported by a module
@tt{m1} but defined in another module @tt{m2} which is used by
@tt{m1}, are normally not documented in the original module, but
instead a simple reference is included to the module in which it is
defined. This can be changed, so that the documentation is included in
the original module, by using a @decl{doc/2} declaration with
@tt{doinclude} in the first argument (see the @lib{comments} library).
This is often useful when documenting a library made of several
components.  For a simple user's manual, it is often sufficient to
include in the @index{documentation configuration} file the
principal module, which is the one which users will do a
@decl{use_module/1} of, in the manual. This module typically exports
or reexports all the predicates which define the library's user
interface.  Note, however, that currently, due to limitations in the
implementation, only the comments inside @concept{assertions} (but not
those in @decl{doc/2} declarations) are included for reexported
predicates.

@subsection{Separating the documentation from the source file}

Sometimes one would not like to include long introductory comments in
the module itself but would rather have them in a different file. This
can be done quite simply by using the @@include command. For example,
the following declaration:

@begin{verbatim}
:- doc(module,""@@include{Intro.lpdoc}"").
@end{verbatim}

@noindent will include the contents of the file @tt{Intro.lpdoc} as
the module description.

Alternatively, sometimes one may want to generate the documentation
from a completely different file. Assuming that the original module is
@tt{mod.pl}, this can be done by calling the module containing the
documentation @tt{mod_doc.pl}. This @tt{mod_doc.pl} file is the one
that will be included in the @index{documentation configuration}
file, instead of @tt{mod.pl}. @apl{lpdoc} recognizes and treats such
@tt{_doc} files specially so that the name without the @tt{_doc} part
is used in the different parts of the documentation, in the same way
as if the documentation were placed in file @tt{mod}.

@subsection{Generating README files}

Using @apl{lpdoc} it is often possible to use a common source for
documentation text which should appear in several places. For example,
assume a file @file{INSTALLATION.lpdoc} contains text describing an
application. This text can be included in a section of the main file
documentation as follows:

@begin{verbatim}
:- doc(module,""
   ...
   @@section{Installation instructions}
   @@include{INSTALLATION.lpdoc}
   ...
   "").
@end{verbatim}

@noindent At the same time, this text can be used to generate a nicely
formatted @tt{INSTALLATION} file in ascii, which can perhaps be included in
the top level of the source directory of the application. To this end, an
@tt{INSTALL.pl} file as follows can be constructed: 

@begin{verbatim}
:- use_package([assertions]).
:- doc(filetype, application). %% forces file to be documented as an application
:- doc(title,""Installation instructions"").
:- doc(module,""@@include{INSTALLATION.lpdoc}"").
@end{verbatim}

Then, the ascii @tt{INSTALLATION} file can be generated by simply
running @tt{lpdoc -t ascii INSTALLATION.pl}.

@subsection{Documenting version/patch changes}

@apl{lpdoc} supports version comments (@tt{:- doc(version(...),
""..."").}) to document the list of version/patch changes
(@concept{CHANGELOG}s) of a particular software. These can be included
as part of the manual or translated to plain text 
(@ref{Generating README files}).

Version numbers in comments specify a @em{major}, @em{minor}, and
@em{patch} number. As a common convention, @em{patch} changes (e.g.,
1.1#2 to 1.1#3) are reserved for internal changes, such as bug fixes
and backward-compatible changes whose detailed description may not be
relevant for the user. More general changes (including the summary of
internal changes when appropriate) are documented with changes in the
@em{major} and @em{minor} numbers (e.g., 1.1#2 to 1.2#0).

Selecting the appropriate options in @apl{lpdoc} (e.g., @tt{no_patch}
in @tt{doc_mainopts}), it is possible to include in the manual the
version changes but not the patch changes (which might on the other
hand be included in an @index{internals manual}). This is useful, for
example, to document separatelly internal from general changes.

@section{Troubleshooting with texinfo-based targets}

Due to limitations in @apl{texinfo} and GNU info, it is sometimes a
little tricky to get things to work uniformly for all formats. The
following recommendations are intended to help in achieving useful
manuals in texinfo-based formats, as well as some common errors and
their usual fix:

@begin{itemize}
@item The GNU info format requires all @em{nodes} (chapters, sections,
etc.) to have different names. This is ensured by @apl{lpdoc} for
the automatically generated sections (by appending the module or file
name to all section headings). However, care must be taken when
writing section names manually to make them different. For example,
use ``lpdoc usage'' instead of simply ``Usage'', which is much more
likely to be used as a section name in another file being documented.

@item Also due to a limitation of the @apl{info} format, do not use
@tt{:} or @tt{,} or @tt{-}@tt{-} in section, chapter, etc. headings.

@item The character ``@tt{_}'' in names may sometimes give problems in
indices, since current versions of @apl{texinfo} do not always handle
it correctly.

@item Messages of the type:
@begin{verbatim}
 ! No room for a new @write .
@end{verbatim} while converting from @tt{.texi} to @tt{.dvi} (i.e.,
while running @apl{tex}). These messages are @apl{tex}'s way of saying
that an internal area (typically for an index) is full. This is
normally because more indices were selected in the @tt{index/1}
option of the @index{documentation configuration} file than the
maximum number supported by the installed version of
@apl{tex}/@apl{texinfo} installations, as mentioned in @ref{Generating
and accessing manuals}. The easiest fix is to reduce the number of indices
generated.  Alternatively, it may be possible to recompile your local
@apl{tex}/@apl{texinfo} installation with a higher number of indices.

@item Missing links in @apl{info} files (a section which exists in the
printed document cannot be accessed in the on-line document) can be
due to the presence of a colon (@tt{:}), a comma (@tt{,}), a double
dash (@tt{--}), or other such separators in a section name. Due to
limitations of @apl{info} section names cannot contain these symbols.

@item Menu listings in @apl{info} which @em{do not work} (i.e., the
menu listings are there, but they cannot be followed): see if they are
indented. In that case it is due to an @tt{itemize} or @tt{enumerate}
which was not closed.

@end{itemize}

").

% :- doc(bug, "Missing documentation for @tt{documentation} filetype. It
% behaves like @tt{part} but does not split the document.").



