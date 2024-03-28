
:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"Generating and accessing manuals").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- doc(module,"

@cindex{generating manuals}
@cindex{manuals generating}

This section describes how to generate a manual (semi-)automatically
from a set of source files using @apl{lpdoc} and how to access it. See
@ref{Writing documentation} for details about writing proper
@apl{lpdoc} documentation.

@section{Basic usage}

@apl{lpdoc} can be used directly from the command line, the supported
IDEs (@apl{Emacs}, @apl{VSC}, the @tt{playground}, etc.), or from
@concept{bundle} manifest files.

The following describes the basic command-line usage and the main
command line options available when invoking @apl{lpdoc}. The basic
usage is:

@begin{verbatim}
lpdoc [Options] Input
@end{verbatim}

where @tt{Input} is the input file (a module or a @index{documentation
configuration} file), and (optional) options specifying the selected
target format (@tt{-t Format}, as described in @pred{docformat/1}),
generation options @tt{--Name=Value} as well as options to view or
clean the generated documentation files.

Use @tt{lpdoc --help}, @ref{Documentation configuration options} and
@ref{Admissible values for the documentation configuration options},
or @lib{doccfg} for a complete list of the available command line and
configuration file options.

@subsection{Generating manuals}

Manuals can be generated from a single module, in which case no
configuration file is needed. More complex documentation can be
generated from a collection of modules, which are then specified in a
@index{documentation configuration} file (generally @tt{SETTINGS.pl}
as described in @ref{Writing documentation}). This file defines how
the documentation is structured, as well as options for its
generation. For example, executing from the @em{command line}:

@begin{verbatim}
lpdoc file.pl
@end{verbatim}

@noindent generates the documentation for @tt{file.pl} in the default
format (@tt{HTML} for single modules or the default formats specified
in @pred{docformat/1} for complex manuals), while the command:

@begin{verbatim}
lpdoc -t pdf file.pl
@end{verbatim}

@noindent generates a PDF manual. The input file can be in several
formats including source @tt{.pl} files and files containing only
markup and/or markdown (@tt{.lpdoc} and @tt{.md} files).  The manuals
generated will generally be written in the same directory as the input
file, and they will have the same name but with the format as
extension (i.e., in the example above it would be @tt{file.pdf}). See
the @pred{output_dir/1} and @pred{output_name/1} options to change the
location or name of the output).

To enable incremental documentation generation, @apl{lpdoc} maintains
intermediate files in a directory named @tt{file.cachedoc/}. See
@ref{Cleaning up documentation} for cleanup commands to remove both
the intermediate and target files.

@cindex{Emacs, generating manuals from}
@cindex{manuals, generating from Emacs}
@cindex{Ciao} 
@cindex{Prolog, Ciao} 
@cindex{Emacs, LPdoc mode} 
@noindent
@comment{Generating a manual from the Ciao Emacs mode:}

@noindent If you use the @apl{Emacs} editor (highly recommended in all
circumstances), then the simplest way to quickly generate a manual is
by doing it from the @concept{Ciao Emacs mode} (this mode comes with
the Ciao distribution and is automatically installed with Ciao). The
Ciao Emacs mode provides menu- and keyboard-binding driven facilities
for generating a standalone document with the documentation
corresponding to the file in the buffer being visited by Emacs.  This
is specially useful while modifying the source of a file, in order to
check the output that will be produced when incorporating this file
into a larger document.

@cindex{VSC, generating manuals from}
@cindex{manuals, generating from VSC}
@cindex{Playground, generating manuals from}
@cindex{manuals, generating from the Playground}
@comment{Generating manuals from other IDEs and from bundles:}

@noindent As mentioned before, you can generate manuals easily also
from the the other supported IDEs such as @apl{VSC}, the
@tt{playground}, etc. which all have buttons and menus for the task.
@concept{Bundle}s can also be configured through their manifest files
to generate manuals automatically from builder commands (@tt{ciao
build --docs}), see the documentation on @lib{bundles} in the Ciao
reference manual.


@subsection{Cleaning up documentation}

@apl{lpdoc} can also take care of tidying up the output of the
documentation generation and the intermediate documenation generation
files, using the following options:

@begin{itemize}

@item @tt{--clean}: deletes all intermediate files, but leaves the
targets (i.e., the @tt{.pdf}, @tt{.ascii}, @tt{.html}, etc. files), as
well as all the generated @tt{.texic} files.

@item @tt{--distclean}: deletes all intermediate files and the
generated @tt{.texic} files, leaving only the targets (i.e., the
@tt{.pdf}, @tt{.ascii}, @tt{.html}, etc. files). This is the option
normally used when building software distributions in which the
manuals come ready made in the distribution itself and will not need
to be generated during installation.

@item @tt{--docsclean}: deletes all intermediate files and the
generated targets, but leaves the @tt{.texic} files. This option can
be used in software distributions in which the manuals in the
different formats will be generated during installation. This is
generally more compact, but requires the presence of several tools,
such as @tt{tex}, @tt{Emacs}, etc., in order to generate the manuals
in the target formats during installation.

@item @tt{--realclean}: performs a complete cleanup, deleting also 
the .texic files, i.e., it typically leaves only the original source
files.  This is is the most compact, but requires the presence of the
tools mentioned above, the source files from which the manuals are
generated and @apl{lpdoc} in order to regenerate the manuals in the
target formats during installation.

@end{itemize}

@section{Accessing manuals} 

Once generated, the documentation can be viewed by opening the target
output with an appriopriate viewer (e.g., Web browser for
@tt{file.html/index.html}, PDF viewer for @tt{file.pdf}, etc.). For
convenience @apl{lpdoc} provides a generic view command:

@begin{verbatim}
lpdoc -t Format --view file.pl
@end{verbatim}

which will open a default viewer application for the specified format
and file.

@begin{note}
For HTML documentation (specially when it is part of
@concept{bundle}s) we encourage the use of the @apl{ciao-serve}
command, which starts a local HTTP server and provides access to
dynamic documentation parts (such as search, and advanced index
options).
@end{note} 

@subsection{Accessing info manuals} 

Generated @tt{.info} files are meant to be viewed by the @apl{Emacs}
editor or by the standalone @apl{info} application, both publicly
available from the GNU project sites. To view the a generated
@apl{info} file from @apl{Emacs} manually (i.e., before it is
installed in a common area), type @tt{C-u M-x info}. This will prompt
for an info file name. @cindex{Emacs, accessing info files} Input the
name of the info file generated by @apl{lpdoc} (@tt{main}@tt{.info})
and @apl{Emacs} will open the manual in info mode.

Automatic, direct on-line access to the information contained in the
info file (e.g., going automatically to predicate descriptions by
clicking on predicate names in programs in an @apl{Emacs} buffer) can
be easily implemented via existing @tt{.el} packages such as
@tt{info-look}, written by Ralph Schleicher
(@email{rs@@ralph-schleicher.de}). Support for this package can be
found in @file{info-look-ciao.el}

@subsection{Accessing man manuals} 

The @apl{Unix} @apl{man} format manuals generated by @apl{lpdoc} can
be viewed using the @apl{Unix} @apl{man} command. In order for
@apl{man} to be able to locate the manuals, they should be copied to
one of the subdirectories (e.g., @tt{/usr/local/man/manl}) of one of
the main man directories (in the previous case the main directory
would be @tt{/usr/local/man}).  As usual, any directory can be used as
as a man main directory, provided it is included in the environment
variable @tt{MANPATH}.

@subsection{Accessing Active Logic Documents} 

@cindex{Active Logic Documents}

Active Logic Documents (ALDs) are standard @apl{lpdoc} documents that
contain runnable examples and queries within them.  These examples and
queries run directly in the browser without requiring any
installation, which greatly facilitates the sharing of ALDs without
external dependiencies.  The @tt{.html} pages for source files
containing such code are generated with @apl{lpdoc} in the standard
way, and they can be viewed in the same way, i.e.:

@begin{verbatim}
lpdoc file.pl
@end{verbatim}

or 

@begin{verbatim}
lpdoc file.md
@end{verbatim}

and 

@begin{verbatim}
lpdoc --view file.html/index.html
@end{verbatim}
").
