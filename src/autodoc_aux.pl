:- module(autodoc_aux, [], [assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Auxiliary definitions").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(lpdoc(autodoc_messages)).
:- use_module(library(system), [file_exists/1]).

:- use_module(lpdoc(autodoc_settings)).

% ---------------------------------------------------------------------------

:- export(all_vars/1).
% All elements of the list are variables
all_vars([]).
all_vars([H|T]) :- var(H), all_vars(T).

% ---------------------------------------------------------------------------

% TODO: replace by a predicate that opens a file, and closes it if
%       fails? call_cleanup?

:- export(read_file/2).
read_file(File, Content) :-
	file_exists(File),
	!,
	open(File, read, IS),
	read_stream(IS, Content),
	close(IS).
read_file(File, []) :-
	autodoc_message(error,"file ~w not found", [File]).

% TODO: duplicate of read_to_end/2?
read_stream(IS, Content) :-
	get_code(IS, N),
	( N = -1 ->
	    Content = []
	; Content = [N|Rest],
	  read_stream(IS, Rest)
	).

% ---------------------------------------------------------------------------

:- export(ascii_blank_lines/2).
ascii_blank_lines(0,"") :- !.
ascii_blank_lines(N,[0'\n | R]) :-
	N1 is N-1,
	ascii_blank_lines(N1,R).

% ---------------------------------------------------------------------------

:- use_module(library(logged_process), [logged_process_call/3]).

% TODO: logs may also be useful when status is 0
% TODO: this really needs a separate flag?
% Options for logging external commands (controlled by verbosity options)
logopts(LogOpts, A) :-
	setting_value_or_default(verbosity,L), L=full, !,
	% In fully verbose mode, always show logs:
	A = [show_logs(always)|LogOpts].
logopts(LogOpts, A) :-
	setting_value_or_default(verbosity,L), L=quiet, !,
	% In quiet mode, no logs:
	A = [show_logs(silent)|LogOpts].
logopts(LogOpts, A) :-
	setting_value_or_default(verbosity,L), L=progress, !,
	% In progress mode (default):
	% A = [show_logs(note_on_error)|LogOpts].
        %%% Alternative value: on_error
	A = [show_logs(on_error)|LogOpts].
        %%% Alternative value: on_error_stderr
	% A = [show_logs(on_error_stderr)|LogOpts].
%% 
%% logopts(LogOpts, A) :-
%% 	!,
%% 	A = [show_logs(on_error)|LogOpts].

:- export(autodoc_process_call/3).
autodoc_process_call(Cmd, Args, Opts) :-
	logged_process_call(Cmd, Args, ~logopts(Opts)).

% ---------------------------------------------------------------------------

:- use_module(lpdoc(autodoc_filesystem), [get_cache_dir0/2]).
:- use_module(library(pathnames), [path_concat/3]).

% LogBase for the given command execution RunId (relative to get_cache_dir0/2)
% (RunId is just a name to identify the command run)
% TODO: Backend should not be needed
:- export(cmd_logbase/3).
cmd_logbase(Backend, RunId, LogBase) :-

	get_cache_dir0(Backend, CacheDir),
	path_concat(CacheDir, RunId, LogBase).

% ---------------------------------------------------------------------------

