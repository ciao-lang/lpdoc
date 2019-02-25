:- module(docmaker, [], [assertions, regtypes, dcg, basicmodes, fsyntax]).

:- doc(title, "Driver for documentation generation").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements the main driver for
   documentation generation.").

:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).

%% Ciao packages and libraries
:- use_module(library(lists), [member/2, list_concat/2, append/3]).
:- use_module(library(stream_utils)).
:- use_module(library(pathnames), [path_concat/3, path_dirname/2, path_splitext/3]).

%% LPdoc libraries
:- use_module(lpdoc(autodoc)).
:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_images), [clean_image_cache/0]).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_texinfo), [infodir_base/2]).
:- use_module(lpdoc(autodoc_messages)).

% Holder for doc_module
:- use_module(lpdoc(docmod_holder)).

:- use_package(fsmemo). % (for 'fsmemo.*') 

% ===========================================================================
:- doc(section, "Commands on Documentation").

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(system), [working_directory/2, cd/1, file_exists/1]).

% TODO: document Opts better, change format (e.g., N(V) instead of get_value(N,V))
:- export(doc_cmd/3).
:- pred doc_cmd(InFile, Opts, Cmd) # "Treat according to @var{Cmd}
   @var{InFile} (which can be a @lib{doccfg}-file or standalone file).
   @var{Opts} is the list of options (see @pred{autodoc_option/1}).".

doc_cmd(InFile0, Opts, Cmd) :-
	% Prepare settings
	doc_input(InFile0, InFile, InKind),
	load_settings(InFile, InKind, Opts),
	% Call
	working_directory(WD, WD),
	once_port_reify(doc_cmd_(InFile, Cmd), Port),
	cd(WD), % move to original directory
	% Cleanup
	clean_docstr,
	clean_tmp_db,
	clean_autodoc_settings,
	port_call(Port).

doc_cmd_(InFile, Cmd) :-
	output_dir(InFile, OutputDir),
	cd(OutputDir),
	verify_settings,
	clean_tmp_db,
	load_vpaths(InFile),
	parse_structure,
	doc_cmd__(Cmd).

doc_input(InFile0, InFile, InKind) :-
	fixed_absolute_file_name(InFile0, '.', InFile1),
	( find_pl(InFile1, InFile),
	  peek_doccfg(InFile) -> % Input is a doccfg file
	    InKind = doccfg
	; file_exists(InFile1) -> % Input is a standalone
	    InFile = InFile1,
	    InKind = standalone
	; throw(autodoc_error("Input file not found: ~w", [InFile0]))
	).

output_dir(InFile, Dir) :-
	( setting_value(output_dir, Dir0) ->
	    Dir = Dir0
	; Dir = ~path_dirname(InFile)
	).

% `Path` is the first file that exists from `Path0` or `Path0` plus `.pl` extension
find_pl(Path0, Path) :-
	( file_exists(Path0) -> Path = Path0
	; atom_concat(Path0, '.pl', Path1),
	  file_exists(Path1),
	  Path = Path1
	).

doc_cmd__(gen(Format)) :- gen(Format).
doc_cmd__(view(Format)) :- view(Format).
doc_cmd__(clean(Mode)) :- clean(Mode).

% TODO: Is this complete?
clean_tmp_db :-
	clean_fs_db,
	clean_image_cache,
	reset_output_dir_db.

gen(Format) :-
	load_doc_modules,
	% report_cmd('Starting', Format),
	gen_actions(Format, Actions),
	fsmemo_call(Actions).
	% report_cmd('Finished', Format).

% (disabled, too verbose)
% report_cmd(BegEnd, Ext) :-
% 	file_format_name(Ext, FormatName),
% 	!,
% 	autodoc_message(progress, "~w manual generation in ~w (~w) format.",
% 	    [BegEnd, Ext, FormatName]).
% report_cmd(BegEnd, Base) :-
% 	autodoc_message(progress, "~w processing of '~w'.", [BegEnd, Base]).

% Load doc_module (for extensions)
% TODO: unload is missing!
% TODO: make it local to each module or lpdoc file instead (allow packages in .lpdoc)
load_doc_modules :-
	( % (failure-driven loop)
	  setting_value(load_doc_module, Spec),
	    docmod_holder:do_use_module(Spec),
	    fail
	; true
	).

% Actions to generate documentation in some specific format
gen_actions(all, Actions) :- !,
	% All (operational) formats
        findall(Action,
	        (setting_value(docformat, Format), % (nondet)
		 action_for_format(Format, Action),
		 action_is_operational(Action)),
		Actions).
gen_actions(Format, Actions) :-
	supported_file_format(Format),
        !,
	action_for_format(Format, Action),
	( action_is_operational(Action) ->
	    Actions = [Action]
	; autodoc_message(error, "Output format '~w' is not operational, please check external dependencies",[Format]),
	  Actions = [] % TODO: propagate error?
	).
gen_actions(Format, _Actions) :-
	autodoc_message(error, "Output format '~w' is not supported.",[Format]). % TODO: propagate error?

% Action that generate one file format (not necessarily requested in the doccfg file)
action_for_format(Format, Action) :-
	parent_format(Format,ParentFormat),
	format_get_subtarget(ParentFormat, Backend, Subtarget),
	( Subtarget = fr -> Action = autodoc_finish(Backend)
	; Subtarget = fr_alt(Alt) -> Action = autodoc_gen_alternative(Backend,Alt)
	; Subtarget = cr -> get_mainmod_spec(Spec), Action = translate_doctree(Backend,Spec)
	; throw(unknown_subtarget(Subtarget))
	).

action_is_operational(Action) :-
	( Action = autodoc_finish(Backend) -> Alt = ''
	; Action = autodoc_gen_alternative(Backend, Alt) -> true
	),
	!,
	autodoc_is_operational(Backend, Alt).
action_is_operational(_).

% ===========================================================================
:- doc(section, "Documentation Generation").

:- use_module(library(pathnames), [path_basename/2]).

% Schematically, there are the rules that define how documentation is
% generated for a specific backend. Let A be a file, Main the mainmod,
% and Ci the components:
%
%   1) dr(A) <- source(A) + <DOCCFG>
%   2) gr(Main) <- [dr(Main), dr(C1),...,dr(Cn)]
%   3) cr(A) <- [gr(Main),dr(A)]
%   4) fr(Main) <- [cr(Main),cr(C1),...,cr(Cn)]
%

:- doc(bug, "Dependency to the DOCCFG file could be refined to its
   contents rather than its date.").

% ---------------------------------------------------------------------------
% 1) Doctree and references from each source

'fsmemo.key'(gen_doctree(Backend,Spec), Target) :- !,
	path_basename(Spec,Name),
	absfile_for_subtarget(Name, Backend, dr, Target).
'fsmemo.deps'(gen_doctree(_Backend, Spec),Deps) :- !,
	query_source(Spec, AbsFile),
	add_settings_dep(AbsFile,Deps).
'fsmemo.run'(gen_doctree(Backend, Spec)) :- !,
        % autodoc_message(simple, "Generating doctree for ~w",[Spec]),
        gen_doctree(Backend, Spec).

gen_doctree(Backend, FileBase) :-
	query_source(FileBase, FileAbs), % TODO: only for FileExt; simplify?
	path_splitext(FileAbs, _, FileExt),
	ensure_cache_dir(Backend),
	path_basename(FileBase, Name),
	get_autodoc_opts(Backend, Name, Opts),
	autodoc_gen_doctree(Backend, FileBase, FileExt, Opts, Name).

% query_source(+Spec, -Path)
query_source(Spec, Path) :-
	find_doc_source(Spec, Path0),
	!,
	Path = Path0.
query_source(Spec, _) :-
	throw(autodoc_error("Source file not found: ~w", [Spec])).

% TODO: Missing dependencies to included files, etc. We need c_itf for this.
add_settings_dep(SpecF) := ['SOURCE'(SpecF)|Fs] :-
	( settings_file(F) ->
	    Fs = ['SOURCE'(F)]
	; Fs = []
	).

% ---------------------------------------------------------------------------
% 2) Globally resolved references

'fsmemo.key'(compute_grefs(Backend), Target) :- !,
	get_mainmod(Mod),
        absfile_for_subtarget(Mod, Backend, gr, Target).
'fsmemo.deps'(compute_grefs(Backend),Deps) :- !,
	get_mainmod_spec(Spec),
	components_target(Backend,dr,FdrComps),
        Deps = [gen_doctree(Backend,Spec)|FdrComps].
'fsmemo.run'(compute_grefs(Backend)) :- !,
        autodoc_message(verbose, "Computing globally resolved references."),
        compute_grefs(Backend).

compute_grefs(Backend) :-
	Mod = ~get_mainmod,
	get_autodoc_opts(Backend, Mod, Opts),
%	display(user_error, computing_grefs(Backend, Mod)), nl(user_error),
	autodoc_compute_grefs(Backend, Mod, Opts).

% ---------------------------------------------------------------------------
% 3) Backend-specific temporary result

'fsmemo.key'(translate_doctree(Backend,Spec), Target) :- !,
	path_basename(Spec,Name),
	absfile_for_subtarget(Name, Backend, cr, Target).
'fsmemo.deps'(translate_doctree(Backend,Spec),Deps) :- !,
        Deps = [gen_doctree(Backend,Spec),compute_grefs(Backend)].
'fsmemo.run'(translate_doctree(Backend,Spec)) :- !,
        % autodoc_message(simple, "Translating doctree for ~w",[Spec]),
        translate_doctree(Backend,Spec).

translate_doctree(Backend, FileBase) :-
	path_basename(FileBase, Base),
%	display(user_error, translating_doctree(Backend, Base)), nl(user_error),
	get_autodoc_opts(Backend, Base, Opts),
	ensure_output_dir_prepared(Backend, Opts),
	autodoc_translate_doctree(Backend, Opts, Base).

% ---------------------------------------------------------------------------
% 4) Backend-specific final result

'fsmemo.key'(autodoc_finish(Backend), Target) :- !,
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, fr, Target).
'fsmemo.deps'(autodoc_finish(Backend),Deps) :- !,
	get_mainmod_spec(Spec),
	components_target(Backend,cr,FcrComps),
        Deps = [translate_doctree(Backend,Spec)|FcrComps].
'fsmemo.run'(autodoc_finish(Backend)) :- !,
        autodoc_message(verbose, "Post-processing the document."),
        autodoc_finish(Backend).

% ---------------------------------------------------------------------------
% (extra) Alternative final results (e.g. PDF, PS, etc.)
% [Rules for generating DVI, PS, and PDF from texi]
'fsmemo.key'(autodoc_gen_alternative(Backend,Alt), Target) :- !,
	get_mainmod(Mod),
	absfile_for_subtarget(Mod, Backend, fr_alt(Alt), Target).
'fsmemo.deps'(autodoc_gen_alternative(Backend,_Alt),Deps) :- !,
        Deps = [autodoc_finish(Backend)].
'fsmemo.run'(autodoc_gen_alternative(Backend,Alt)) :- !,
        autodoc_message(verbose, "Generating document in ~w format.",[Alt]),
        autodoc_gen_alternative(Backend,Alt).

% ---------------------------------------------------------------------------

components_target(Backend, _Subtarget, []) :- backend_ignores_components(Backend), !.
components_target(Backend, Subtarget, CompTargets) :-
	CompTargets = ~targets(~all_component_specs, Backend, Subtarget).

targets([],           _, _) := [].
targets([FileBase|FileBases], Backend, Subtarget) := [X|Xs] :-
	( Subtarget = dr -> X = gen_doctree(Backend,FileBase)
	; Subtarget = cr -> X = translate_doctree(Backend,FileBase)
	; fail
	),
	Xs = ~targets(FileBases, Backend, Subtarget).

% ===========================================================================
:- doc(section, "Visualization").

:- use_module(library(opendoc), [opendoc/1]).

view(Format) :-
	get_mainmod(Mod),
	format_get_file(Format, Mod, File),
	( Format = html -> Target = ~atom_concat('file://', File) % TODO: use localhost and ciao-serve?
	; Target = File
	),
	opendoc(File).

% ===========================================================================
:- doc(section, "Cleaning up").
% (after generating manuals)

clean(intermediate) :- clean_intermediate. % leaves targets and .texi
clean(docs_no_texi) :- clean_docs_no_texi. % leaves only .texi
clean(all_temporary) :- clean_all_temporary. % leaves only targets
clean(all) :- clean_all. % leaves nothing

% ===========================================================================

% Detect if file is a doccfg module based on the first term
% (see source_tree.pl)

:- use_module(engine(stream_basic)).
:- use_module(library(read), [read/2]).

peek_doccfg(FileName) :-
	% TODO: safety check (read/2 runs out of memory on wrong files)
	atom_concat(_, '.pl', FileName),
	%
	read_first_term(FileName, Term),
	( var(Term) -> fail
	; Term = (:- module(_, _, Ps)),
	  member(doccfg, Ps) ->
	    true
	; Term = (:- doccfg(_)) -> % TODO: arity 0 or 1?
	    true
	; fail
	).

% Read the first term (leave unbound on failure or exception)
read_first_term(File, Term) :-
	catch(open(File, read, Stream), _, fail), % (it may be a broken symlink)
	( catch(read(Stream, Term), _, true) ->
	    true
	; true
	),
	close(Stream).
