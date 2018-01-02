:- module(lpdoc_single_mod, [], [assertions, dcg, fsyntax]).

:- doc(title, "Generate single module documentation").
% TODO: this code still needs some cleanups, add to LPdoc manual

:- doc(module, "This module generates and locates the documentation
   for a module or predicate, even if that module is not part of an
   existing manual.").

:- use_module(library(pathnames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(persdb/datadir), [ensure_datadir/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/4]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(pillow/html), [html2terms/2]).

:- use_module(lpdoc(docmaker), [doc_cmd/3]).
:- use_module(lpdoc(autodoc_lookup), [html_doc_file/2]).

% ---------------------------------------------------------------------------
% Data directory where documentation files are generated (for modules
% whose documentation is not part of a manual)

tmp_doc_path := P :-
	P = ~ensure_datadir('tmp_doc').

% ---------------------------------------------------------------------------

:- export(lpdoc_single_mod/1).
:- pred lpdoc_single_mod(Spec)
   # "Ensure that the HTML documentation for the @var{Spec} module has
      been generated (either as part of a manual or as single module
      documentation at the @tt{tmp_doc} data directory)".

% TODO: if documentation already exists, it is not updated!
% TODO: update manual if needed? (it may be slow)
lpdoc_single_mod(Spec) :-
	( DocPath0 = ~ext_html_doc_file(Spec) ->
	    DocPath = DocPath0
	; % Generate at tmp_doc
	  ModPath = ~absolute_file_name(Spec),
	  OutDir = ~tmp_doc_path,
	  doc_cmd(ModPath, [name_value(output_dir, OutDir)], gen(html)),
	  DocPath = ~lpdoc_single_mod_path(Spec)
	).

:- export(lpdoc_single_mod_embedded/1).
:- pred lpdoc_single_mod_embedded(Spec)
   # "Like @pred{lpdoc_single_mod/2} but using embedded HTML layout
     (omit sections, title, etc.)".

lpdoc_single_mod_embedded(Spec) :-
	ModPath = ~absolute_file_name(Spec),
	OutDir = ~tmp_doc_path,
	doc_cmd(ModPath, [name_value(output_dir, OutDir),
	                  name_value(html_layout, embedded),
			  name_value(allow_markdown, yes),
			  name_value(syntax_highlight, yes)
%			  name_value(syntax_highlight, no)
			 ],
                gen(html)).

:- export(lpdoc_single_mod_embedded_path/2).
lpdoc_single_mod_embedded_path(Spec) := ~lpdoc_single_mod_path(Spec).

:- export(ext_html_doc_file/2).
:- pred ext_html_doc_file(Spec, DocPath)
   # "Like @pred{html_doc_file/2} but it tries to lookup documentation
      using @pred{lpdoc_single_mod_path/2} as fallback.".

ext_html_doc_file(Spec) := DocPath :-
	( DocPath = ~html_doc_file(Spec) ->
	    true
	; DocPath = ~lpdoc_single_mod_path(Spec),
	  file_exists(DocPath)
	).

% TODO: add some lpdoc option to generate embedded doc or use lpdoc as service
lpdoc_single_mod_path(Spec) := DocPath :-
	ModPath = ~absolute_file_name(Spec),
	path_splitext(ModPath, ModBase, '.pl'), % remove '.pl'
	path_split(ModBase, _ModDir, ModName),
	OutDir = ~tmp_doc_path,
	ModDocLoc = ~atom_concat(~path_concat(OutDir, ModName), '.html'),
	% generate doc link % TODO: bug, fix that the name of the doc file is intro
	HtmlFile = ~atom_concat(ModName, 'intro.html'), % html file
	DocPath = ~path_concat(ModDocLoc, HtmlFile). % full path to html

% ---------------------------------------------------------------------------

:- export(docfile_to_html_term/2).
:- pred docfile_to_html_term(ModPath, HtmlTerm) # "Obtain the
   documentation for @var{ModPath} as an HTML term (see
   @lib{pillow/html})".

docfile_to_html_term(ModPath, HtmlTerm) :-
	OutDir = ~tmp_doc_path,
	path_splitext(ModPath, ModBase, _), % remove extension
	path_split(ModBase, _, ModName),
	doc_cmd(ModPath, [name_value(output_dir, OutDir)],
                clean(all)),
	doc_cmd(ModPath, [name_value(output_dir, OutDir),
	                  name_value(html_layout, embedded),
			  name_value(allow_markdown, yes),
			  name_value(syntax_highlight, yes)
%			  name_value(syntax_highlight, no)
			 ],
                gen(html)),
	%
	MainName = ~atom_concat(ModName, '.html'),
	path_concat(OutDir, MainName, OutDir2),
	path_concat(OutDir2, MainName, HtmlFile),
	file_to_string(HtmlFile, Content),
	html2terms(Content, HtmlTerm),
	%
	doc_cmd(ModPath, [name_value(output_dir, OutDir)],
                clean(all)).

% ---------------------------------------------------------------------------

:- export(add_pred_anchor_url/4). % TODO: see rw_command/3 for defpred/5
% Obtain a pred URL from a module URL
add_pred_anchor_url(ModURL, Name, Arity) := PredURL :-
	atom_number(AArity, Arity),
	atom_concat([ModURL, '#', Name, '/', AArity], PredURL).

% ---------------------------------------------------------------------------
% URL relative to build/site

:- export(get_pred_doc_url/5).
:- pred get_pred_doc_url(+ModPath, +PredName, +A, -ModRef, -PredRef)
   # "@var{ModRef} and @var{PredRef} are the URL (relative to
      @tt{build/site}) in the documentation of @var{PredName}/@var{A}
      in @var{ModPath}".

get_pred_doc_url(ModPath, Name, Arity, ModURL, PredURL) :-
	ModURL0 = ~ext_html_doc_file(ModPath), % may generate doc files!!
	( get_mod_doc_url(ModURL0, ModURL) ->
	    true
	; ModURL = ModURL0 % TODO: incorrect?
	),
	PredURL = ~add_pred_anchor_url(ModURL, Name, Arity).

:- export(get_mod_doc_url/2).
% Obtain URL (under build/site) from a path to a documentation file
% (generated under build/doc or data/tmp_doc)
get_mod_doc_url(Path) := HRef :-
	bundle_path(core, builddir, 'doc', BuildDirDoc), % TODO: hardwired bundle
	% Obtain relative URL
	path_relocate(BuildDirDoc, '', Path, HRef0),
	%
	path_concat('/ciao/build/doc', HRef0, HRef).
get_mod_doc_url(Path) := HRef :-
	BuildDirDoc = ~tmp_doc_path,
	% Obtain relative URL
	path_relocate(BuildDirDoc, '', Path, HRef0),
	% (this points to a symlink at build/site)
	path_concat('/tmp_doc', HRef0, HRef).

