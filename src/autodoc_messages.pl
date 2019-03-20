:- module(autodoc_messages,
	[
	    autodoc_message/2,
	    autodoc_message/3,
	    autodoc_message/4
	], [assertions, regtypes, fsyntax]).

:- doc(title, "Messages library for lpdoc").
:- doc(author, "Manuel Hermenegildo").

:- doc(module, "
   @cindex{lpdoc messages}

   This library provides a message printing mechanism similar to the
   error, warning, note, etc. messages provided by the standard Ciao
   libraries, but adding the possibility of turning off messages that
   are at or below a certain level. This is useful so that warnings
   can be increased during documentation development (e.g., 'missing
   author') but can be turned off, e.g., during normal builds and
   installations. The level is controlled with the @apl{lpdoc}
   @tt{warning_level} and @tt{verbosity} settings.

").

:- doc(bug,"This functionality should perhaps be added instead to the
   message libraries, with a module-based application mechanism (as
   debug_message). In any case this module can be a stepping stone in
   that direction.").

:- set_prolog_flag(multi_arity_warnings, off).

% ---------------------------------------------------------------------------

:- reexport(library(compiler/c_itf), [location_t/1]).
:- reexport(library(format),[format_control/1]).

:- use_module(engine(stream_basic), [sourcename/1]).

:- use_module(lpdoc(autodoc_settings),[setting_value_or_default/2,autodoc_option/1]).

% Ciao libraries
:- use_module(library(messages),[
	show_message__/5, % internal
	message_t/1
	]).
%% The verbosity of these is already controlled by other mechanisms: reexport.
%% Perhaps provide alternatives (e.g., for -v, which is somewhat ad-hoc.
%% Not really needed after using the primitives here insttead of docst_message?
%% :- reexport(library(messages),[
%% 	optional_message/2,
%% 	optional_message/3,
%% 	debug_message/1,
%% 	debug_message/2
%%         ]).

% ---------------------------------------------------------------------------
% Message levels
% ---------------------------------------------------------------------------

% TODO: MH Same as in doccfg_props.... 
:- doc(verbosity_t/1, "Defines the levels of verbosity for the
   messages produced by the message printing
   predicates. @includedef{verbosity_t/1}").
:- export(verbosity_t/1).
:- regtype verbosity_t(L) # "@var{L} is a verbosity level.".

verbosity_t := quiet|progress|full.


:- doc(warning_level_t/1, "Defines which levels of warning will be
   printed by the message printing
   predicates. @includedef{warning_level_t/1}").
:- export(warning_level_t/1).
:- regtype warning_level_t(L) # "@var{L} is a warning level.".

warning_level_t := none|normal|all.


:- doc(autodoc_message_t/1, "The types of message supported by the
   message @lib{autodoc} printing predicates. Basically: 

@begin{itemize}

@item @tt{error}: Error message, will be printed always.

@item @tt{warning}: Warning message, will be printed if the
@tt{warning} setting is @tt{normal} (default) or @tt{all} (default or
@tt{-v}).

@item @tt{note}: Note message, will be printed if @tt{warning=all}
(@tt{-v}).

@item @tt{progress}: Progress message, will be printed if
@tt{verbosity=progress} (default) or @tt{verbosity=verbose}.  These
are intended to be the minimal progress messages (e.g., one for each
source file processed) so that the system is not silent for a very
long time when processing a large document. If @tt{verboisty=silent}
(@tt{'-q'}) is defined, these messages are not printed.


@item @tt{verbose}: Progress message, will be printed only if
@tt{verbosity=verbose} (@tt{'-v'}). They are intended to be additional
progress messages that are useful during document development of
trouble shooting.


@end{itemize}


").

:- regtype autodoc_message_t(M) # "@var{M} is a type of message supported
   by the message printing predicates.".

autodoc_message_t(error).
autodoc_message_t(warning).
autodoc_message_t(note).
autodoc_message_t(progress).
autodoc_message_t(verbose).

% ---------------------------------------------------------------------------

:- pred autodoc_message(Type, Text) : autodoc_message_t * string

# "The text provided in @var{Text} is printed as a message of type
   @var{Type} (if allowed by the current error reporting level).".

:- meta_predicate autodoc_message(?, addmodule).

autodoc_message(Type, Message, Module) :-
	optional_show_message__(Type, _, Message, [], Module).


:- pred autodoc_message(Type, Text, ArgList)
	: autodoc_message_t * format_control * list

# "The text provided in @var{Text} is printed as a message of type
   @var{Type} (if allowed by the current error reporting level), using
   the arguments in @var{ArgList} to interpret any variable-related
   formatting commands embedded in @var{Text}.".

:- meta_predicate autodoc_message(?, ?, addmodule).

autodoc_message(Type, Message, A, Module) :-
	optional_show_message__(Type, _, Message, A, Module).


:- pred autodoc_message(Type, Lc, Text, ArgList)
	: autodoc_message_t * location_t * format_control * list

# "The text provided in @var{Text} is printed as a message of type
   @var{Type}, using the arguments in @var{ArgList} to interpret any
   variable-related formatting commands embedded in @var{Text}, and
   reporting error location @var{Lc} (file and line numbers).".

:- meta_predicate autodoc_message(?, ?, ?, addmodule).

autodoc_message(Type, Loc, Message, A, Module) :-
	optional_show_message__(Type, Loc, Message, A, Module).


%% ---------------------------------------------------------------------------

:- pred optional_show_message__(Type, Loc, Text, ArgList, Module)
	: autodoc_message_t * location_t * format_control * list * sourcename
        # "Does the work.".
 
optional_show_message__(Type, Loc, Message, A, Module) :-
	( message_should_show(Type,LibType) -> 
	  show_message__(LibType, Loc, Message, A, Module)
	; true ).

:- pred message_should_show(Type,LibType)
	: autodoc_message_t(Type) => message_t(LibType).

message_should_show(error,error). % Errors should always show. 
message_should_show(warning,warning) :-
	setting_value_or_default(warning_level,L), (L=normal;L=all).
message_should_show(note,note) :- 
	setting_value_or_default(warning_level,L), L=all.
message_should_show(progress,simple) :- 
	setting_value_or_default(verbosity,L), (L=progress;L=full).
message_should_show(verbose,simple) :- 
	setting_value_or_default(verbosity,L), L=full.

%% ---------------------------------------------------------------------------

