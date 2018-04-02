:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LPdoc").

% % ===========================================================================
% :- doc(section, "Configuration rules").
%
% TODO: Add here detection of texinfo, makeinfo, etc.

% ============================================================================
:- doc(section, "Build rules").

'$builder_hook'(prepare_build_bin) :-
	generate_version_auto_lpdoc.

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(builder_aux), [generate_version_auto/2]).

generate_version_auto_lpdoc :-
	Bundle = lpdoc,
	File = ~bundle_path(Bundle, 'src/version_auto.pl'),
	generate_version_auto(Bundle, File).

% ===========================================================================
:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	runtests_dir(lpdoc, 'src').


