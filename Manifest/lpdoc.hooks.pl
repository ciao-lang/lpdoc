:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LPdoc").

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
:- doc(section, "Tests, benchmarks, analysis").

:- use_module(library(system), [working_directory/2]).
:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).
:- use_module(ciaobld(ciaopp_aux), [invoke_ciaopp/1, invoke_ciaopp_batch/1, invoke_ciaopp_dump/1]).

'$builder_hook'(test) :- !,
    runtests_dir(lpdoc, 'src').

'$builder_hook'(analyze) :- !,
    working_directory(ThisDir, ThisDir),
    working_directory(_, ~bundle_path(lpdoc, '.')),
    invoke_ciaopp_batch(['pdb', 'src']),
    working_directory(_, ~bundle_path(lpdoc, 'cmds')),
    invoke_ciaopp(['-A', 'lpdoccl.pl', '-ftypes=none', '-fmodes=pdb', '-fintermod=on', '-fmenu_dump=incremental', '-fmenu_output=off']),
    invoke_ciaopp_dump([report, reach, 'lpdoccl.pl.dump']),
    working_directory(_, ThisDir).


