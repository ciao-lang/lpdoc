:- module(autodoc_settings, [], [dcg, assertions, regtypes, fsyntax, datafacts]). 

:- doc(title, "Current documentation settings").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines the predicates to load and access
   documentation configurations (modules implementing
   @lib{doccfg}).").

:- use_module(engine(stream_basic), [fixed_absolute_file_name/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(aggregates)).
:- use_module(lpdoc(autodoc_messages),[autodoc_message/3]).

% ---------------------------------------------------------------------------

% TODO: Merge autodoc_option/1 with name_value/2?
:- export(autodoc_option/1).
:- data autodoc_option/1.
%
:- export(settings_file/1).
% settings_file(F): F is the absolute file name with settings (doccfg)
% (none for standalone)
:- data settings_file/1.
%
:- data name_value/2.

set_settings_file(ConfigFile) :-
    assertz_fact(settings_file(ConfigFile)).

:- export(clean_autodoc_settings/0).
clean_autodoc_settings :-
    retractall_fact(settings_file(_)),
    retractall_fact(autodoc_option(_)),
    retractall_fact(name_value(_, _)).

set_opts([]).
set_opts([X|Xs]) :- set_opt(X), set_opts(Xs).

set_opt(autodoc_option(Opt)) :- !,
    assertz_fact(autodoc_option(Opt)).
set_opt(name_value(Name, Value)) :- !,
    add_name_value(Name, Value).
set_opt(X) :- throw(error(unknown_opt(X), set_opt/1)).

% ---------------------------------------------------------------------------
% NOTE: 
%   Any name_value/2 value overwrides **all** the doccfg values for
%   that setting.

:- use_module(lpdoc(doccfg_holder)).
:- use_module(library(lists), [member/2, append/3]).

add_name_value(Name, Value) :-
    assertz_fact(name_value(Name, Value)).

% read all values
% TODO: findall of get_value should be equivalent
all_values(Name, Values) :-
    all_name_values(Name, Values0),
    ( Values0 = [] ->
        all_pred_values(Name, Values)
    ; Values = Values0
    ).

all_pred_values(Name, Values) :-
    findall(Value, get_pred_value(Name, Value), Values).

all_name_values(Name, Values) :-
    findall(Value, name_value(Name, Value), Values).

get_value(Name, Value) :-
    ( name_value(Name, _) ->
        name_value(Name, Value)
    ; get_pred_value(Name, Value)
    ).

% Just keep one config file at a time
:- data prev_config_file/1.

dyn_load_doccfg(ConfigFile) :-
    ( retract_fact(prev_config_file(PrevConfigFile)) ->
        doccfg_holder:do_unload(PrevConfigFile)
    ; true
    ),
    doccfg_holder:do_use_module(ConfigFile),
    assertz_fact(prev_config_file(ConfigFile)).

% (Get value, given by a predicate definition Name/1)
get_pred_value(Name, Value) :-
    ( atom(Name) ->
        Pred =.. [Name, Value]
    ; Name =.. Flat,
      append(Flat, [Value], PredList),
      Pred =.. PredList
    ),
    doccfg_holder:call_unknown(_:Pred).

% ---------------------------------------------------------------------------
:- doc(section, "Loading Settings").

% TODO: no unload?
:- export(load_settings/3).
:- pred load_settings(InFile, InKind, Opts) # "Set configuration from
   @var{InFile} of kind @var{InKind} and @var{Opts}".

load_settings(InFile, InKind, Opts) :-
    clean_autodoc_settings,
    fixed_absolute_file_name(InFile, '.', InFile2),
    ( InKind = doccfg ->
        ( dyn_load_doccfg(InFile2) -> true
        ; throw(autodoc_error("could not load doccfg file ~w", [InFile]))
        ),
        set_settings_file(InFile2)
    ; InKind = standalone ->
        path_dirname(InFile2, InDir),
        % Fill cfg for standalone
        add_name_value(filepath, InDir),
        add_name_value(docformat, html), % default format
        add_name_value('$implements', 'doccfg'),
        ( member(name_value(doc_mainopts, biblio), Opts) -> % TODO: generalize
            true
        ; add_name_value(doc_mainopts, no_biblio) % (default, use option to enable)
        ),
        add_name_value(doc_structure, [InFile]) % TODO: or InFile2?
    ; fail
    ),
    set_opts(Opts),
    ensure_lpdoc_etc_defined.

% Verify that the configuration module uses the library(doccfg) package
:- export(verify_settings/0).
verify_settings :-
    ( setting_value('$implements', 'doccfg') ->
        true
    ; throw(autodoc_error("Configuration files must use the library(doccfg) package", []))
    ).

% Define 'lpdoc_etc' setting, check that it is valid
ensure_lpdoc_etc_defined :-
    ( LpDocEtcDir = ~file_search_path(lpdoc_etc),
      file_exists(~path_concat(LpDocEtcDir, 'lpdoc.css')) -> % (some example)
        add_name_value(lpdoc_etc, LpDocEtcDir)
    ; autodoc_message(error, 
% ___________________________________________________________________________
 "No valid file search path for 'lpdoc_etc' alias.\n"||
 "Please, check that LPdoc is installed properly.\n", []),
      fail
    ).

%:- multifile file_search_path/2.
%:- dynamic(file_search_path/2). % (just declaration, dynamic not needed in this module)

:- use_module(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------
:- doc(section, "Checking or Setting Options").

:- use_module(library(system)).
:- use_module(library(bundle/bundle_paths), [bundle_path/4]).

% (With implicit default value)
:- export(setting_value_or_default/2).
:- pred setting_value_or_default(Var, Value)
# "Returns in @var{Value} the value of the variable @var{Var}. In case
  this variable does not exist, it returns a default value. If there
  is no default value for the variable @var{Var} it fails.".

setting_value_or_default(Name, Value) :-
    ( get_value(Name, _) -> % Has some value
        get_value(Name, Value)
    ; ( Value0 = ~default_val(Name) ->
          Value = Value0
      ; autodoc_message(error,
          "No ~w provided and no default value available", [Name]),
        fail
      )
    ). 
    

% TODO: Use defaults from doccfg package instead?
default_val(startpage) := 1.
default_val(papertype) := afourpaper.
default_val(verbosity) := progress.
default_val(warning_level) := normal.

:- export(setting_value/2).
setting_value(Name, Value) :-
    get_value(Name, Value).

:- export(all_setting_values/2).
all_setting_values(Name) := ~all_values(Name).

% ---------------------------------------------------------------------------
:- doc(section, "Paths to files").

% TODO: Reuse the logic to locate modules by the compiler?

:- use_module(library(bundle/bundle_paths), [ext_absolute_file_name/3]).
:- use_module(library(pathnames), 
    [path_dirname/2, path_is_absolute/1, path_norm/2, path_concat/3]).
:- use_module(lpdoc(autodoc_filesystem), [cleanup_vpath/0, add_vpath/1]).

:- export(load_vpaths/1).
% Setup vpath values, relative to directory of InFile if needed
load_vpaths(InFile) :-
    cleanup_vpath,
    path_dirname(InFile, InDir),
    ( % (failure-driven loop)
      ( P = InDir
      ; resolved_filepath(InDir, P)
      ; file_search_path(_Alias, P), % TODO: prioritize path aliases for the current bundle?
        \+ P = '.'
      ),
        add_vpath(P),
        fail
    ; true
    ).

:- multifile file_search_path/2.
:- dynamic(file_search_path/2). % (just declaration, dynamic not needed in this module)

% Obtain a resolved filepath (use ext_absolute_file_name/2 and make it relative to InDir if needed)
resolved_filepath(InDir, P) :-
    % TODO: document at_bundle(_,_) syntax?
    member(P0, ~all_setting_values(filepath)),
    ext_absolute_file_name(P0, InDir, P1),
    ( path_is_absolute(P1) -> P = P1
    ; path_concat(InDir, P1, P2),
      path_norm(P2, P)
    ).

% TODO: prioritize path aliases for the current bundle?
% :- use_module(lpdoc(autodoc_filesystem), [get_parent_bundle/1]).
% :- use_module(engine(internals), ['$bundle_alias_path'/3]).

% ---------------------------------------------------------------------------

:- export(custom_html_layout/0).
custom_html_layout :-
    setting_value(html_layout, Layout),
    ( Layout = website_layout(_) -> true
    ; Layout = tmpl_layout(_, _, _)
    ).

% ---------------------------------------------------------------------------
:- doc(section, "External Commands").
% TODO: Ideally, each backend should specify this part.

:- doc(subsection, "Bibliography Generation").

%% The command that builds .bbl files from .bib bibliography
%% files in your system
:- export(bibtex/1).
bibtex := 'bibtex'.

:- doc(subsection, "Texinfo Related Commands").

%% Define this to be the command that runs tex in your system
:- export(tex/1).
tex := 'tex'.

%% The command that runs texindex in your system
:- export(texindex/1).
texindex := 'texindex'.

%% (Awk for modern texindex.awk versions)
:- export(awk/1).
awk := 'awk'.

%% The command that converts dvi to postscript in your system.
:- export(dvips/1).
dvips := 'dvips'.

%% The command that converts postscript to pdf in your system. Make
%% sure it generates postscript fonts, not bitmaps (selecting -Ppdf in
%% dvips often does the trick)
:- export(ps2pdf/1).
ps2pdf := 'ps2pdf'.

%% The command that converts tex to pdf in your system
%% texpdf := 'pdftex'.

%% The command that converts texinfo files into info
%% files in your system. Set also the appropriate flags.
:- export(makeinfo/1).
makeinfo := 'makeinfo'.

:- doc(subsection, "Image Conversions").

%% The command that converts graphics files to other formats
:- export(convertc/1).
convertc := 'convert'.

