:- module(_, [], [regtypes, assertions, fsyntax]).

:- doc(title, "Properties for documentation settings").

:- doc(module, "This module defines several properties used in
   documentation configuration settings.").

:- doc(section, "Paths").

:- export(dirpath/1).
:- doc(dirpath/1, "An atom describing a path to a
   directory. Should be a full, explicit path (i.e., not containing
   environment variables).").

:- regtype dirpath(P) # "@var{P} is a full path to a directory.".

dirpath(P) :- atm(P).

:- export(filename/1).
:- regtype filename(P) # "@var{P} is a full path to a file.".

filename(P) :- atm(P).

:- doc(section, "Options for documentation generation").

:- export(supported_option/1).
:- regtype supported_option/1
   # "Possible options: @includedef{supported_option/1}".

:- doc(supported_option/1, "In general, selecting none of these
	options generates the most verbose manuals, i.e., each option
	generally supresses the production of a certain kind of output
	(on the other hand, @tt{'verbose'} selects verbose output from
	@apl{ciaoc} when processing the file).").

% TODO: Duplicated in lpdoc/src/autodoc_state.pl
supported_option := verbose.
supported_option := no_bugs.
supported_option := no_authors.
supported_option := no_stability.
supported_option := no_version.
supported_option := versioned_output.
supported_option := no_lpdocack.
supported_option := no_changelog.
supported_option := no_patches.
supported_option := autogen_warning.
supported_option := modes.
supported_option := head_props.
supported_option := literal_props.
supported_option := no_propnames.
supported_option := no_undefined.
supported_option := no_propsepln.
supported_option := no_biblio.
supported_option := no_sysmods.
supported_option := no_engmods.
supported_option := no_packages.
supported_option := no_isoline.
supported_option := propmods.
supported_option := no_propuses.
supported_option := shorttoc.
supported_option := regtype_props.
supported_option := onesided.
supported_option := no_math.
supported_option := tests.
supported_option := no_examples.
supported_option := status.

:- doc(section, "Supported formats").

:- export(supported_format/1).
:- regtype supported_format/1
   # "Available formats: @includedef{supported_format/1}".

supported_format := texi.
supported_format := dvi.
supported_format := ps.
supported_format := pdf.
supported_format := ascii.
supported_format := manl.
supported_format := info.
supported_format := html.

:- doc(section, "Indices").

:- export(supported_index/1).
:- regtype supported_index/1
   # "Supported indexes: @includedef{supported_index/1}".

supported_index := concept.
supported_index := lib.
supported_index := apl.
supported_index := pred.
supported_index := prop.
supported_index := regtype.
supported_index := decl.
supported_index := op.
supported_index := modedef.
supported_index := file.
supported_index := global.
supported_index := all.

:- doc(section, "Types of paper").

:- export(supported_papertype/1).
:- regtype supported_papertype/1
# "Possible papertypes: @includedef{supported_papertype/1}".

supported_papertype := letterpaper.
supported_papertype := smallbook.
supported_papertype := afourpaper.
supported_papertype := afourlatex.
supported_papertype := afourwide.
supported_papertype := afourthesis.

:- doc(section, "Other properties").

:- export(yesno/1).
:- regtype yesno/1
# "Enumerated type: @includedef{yesno/1}".

yesno := yes|no.


% TODO: MH Same as in autodoc_messages.... 
:- doc(verbosity_t/1, "Defines the levels of verbosity for the
   messages produced by the message priting
   predicates. @includedef{verbosity_t/1}").
:- export(verbosity_t/1).
:- regtype verbosity_t(L) # "@var{L} is a verbosity level.".

verbosity_t := quiet|progress|full.


:- doc(warning_level_t/1, "Defines which levels of warning will be
   printed by the message priting
   predicates. @includedef{warning_level_t/1}").
:- export(warning_level_t/1).
:- regtype warning_level_t(L) # "@var{L} is a warning level.".

warning_level_t := none|normal|all.


