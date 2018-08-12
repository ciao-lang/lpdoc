
:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"Generating and accessing manuals").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Jose F. Morales").

:- doc(module,"

@cindex{generating manuals}

This section describes how to generate a manual (semi-)automatically
from a set of source files using @apl{lpdoc} and how to access it. See
@ref{Writing documentation} for details about writing proper
@apl{lpdoc} documentation.

@section{Basic usage}

@apl{lpdoc} can be used directly from the command line, the
@apl{Emacs} editor, or from @concept{bundle} manifest files (see
@lib{bundles} in the Ciao reference manual).

The following provides the basic command-line usage and the main
command line options available when invoking @apl{lpdoc}. The basic
usage is:

@begin{verbatim}
lpdoc [Options] Input
@end{verbatim}

where @tt{Input} is the input file (a module or a @index{documentation
configuration} file), and (optional) options specifying the selected
target format (@tt{-t Format}, as described in @pred{docformat/1}),
generation options @tt{--Name=Value} (see @lib{doccfg} for a complete
list), as well as options to view or clean the generated documentation
files.

Use @tt{lpdoc --help} to see a complete list of the available command
line options.

@subsection{Generating manuals}

Manuals can be generated from single module or from a collection of
modules specified in a @index{documentation configuration} file (as
described in @ref{Writing manuals}) which defines how the
documentation is structured, as well as options for its
generation). For example, executing from the command line:

@begin{verbatim}
lpdoc file.pl
@end{verbatim}

@noindent generates the documentation for @tt{file.pl} in the default
format (@tt{HTML} for single modules or the default formats specified
in @pred{docformat/1} for complex manuals), while the command:

@begin{verbatim}
lpdoc -t pdf file.pl
@end{verbatim}

@noindent generates a PDF manual. The manuals generated will generally
be written in the same directory as the input file, and they will have
the same name but with the format as extension (i.e., in the example
above it would be @tt{file.pdf}). See @pred{output_dir/1} and
@pred{output_name/1} options to change the location or name of the
output).

To enable incremental documentation generation, @apl{lpdoc} maintains
intermediate files in a directory named @tt{file.cachedoc/}. See
@ref{Cleaning up documentation} for cleaup commands to remove both the
intermediate and target files.

@cindex{Emacs, generating manuals from}
@cindex{generating from Emacs}
@cindex{Ciao} 
@cindex{Prolog, Ciao} 
@cindex{Emacs, LPdoc mode} 
@noindent @textbf{Generating a manual from the Ciao Emacs mode:} If
you use the @apl{Emacs} editor (highly recommended in all
circumstances), then the simplest way to quickly generate a manual is
by doing it from the @concept{Ciao Emacs mode} (this mode comes with
the Ciao distribution and is automatically installed with Ciao). The
Ciao Emacs mode provides menu- and keyboard-binding driven facilities
for generating a standalone document with the documentation
corresponding to the file in the buffer being visited by Emacs.  This
is specially useful while modifying the source of a file, in order to
check the output that will be produced when incorporating this file
into a larger document.

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

@item @tt{--realclean}: performs a complete cleanup, deleting also the
.texic files, i.e., it typically leaves only the original source
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

@begin{alert}
For HTML documentation (specially when it is part of
@concept{bundle}s) we encourage the use of the @apl{ciao-serve}
command, which starts a local HTTP server and provides access to
dynamic documentation parts (such as search, and advanced index
options).
@end{alert} 

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
@tt{word-help}, written by Jens T. Berger Thielemann
(@email{jensthi@@ifi.uio.no}). @file{word-help} may already be in your
@apl{Emacs} distribution, but for convenience the file
@file{word-help.el}, providing suitable initialization are included in
the @tt{lpdoc} library.  A suitable interface for @file{word-help} is
also provided by the @tt{ciao.el} @apl{Emacs} file that comes with the
@apl{Ciao} system distribution (i.e., if @tt{ciao.el} is loaded it is
not necessary to load or initialize @tt{word-help}).

@subsection{Accessing man manuals} 

The @apl{Unix} @apl{man} format manuals generated by @apl{lpdoc} can
be viewed using the @apl{Unix} @apl{man} command. In order for
@apl{man} to be able to locate the manuals, they should be copied to
one of the subdirectories (e.g., @tt{/usr/local/man/manl}) of one of
the main man directories (in the previous case the main directory
would be @tt{/usr/local/man}).  As usual, any directory can be used as
as a man main directory, provided it is included in the environment
variable @tt{MANPATH}.

").
