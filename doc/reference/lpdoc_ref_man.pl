:- module(lpdoc_ref_man, [], [assertions]).

:- doc(filetype, application).

:- doc(title, "The lpdoc Documentation Generator").
:- doc(subtitle, "An Automatic Documentation Generator for (C)LP Systems").

:- doc(logo, 'lpdoc-logo-128').

:- doc(subtitle_extra, "REFERENCE MANUAL").
:- doc(subtitle_extra, "@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra, "@href{https://ciao-lang.org/}").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").
:- doc(subtitle_extra, "Technical Report CLIP 5/97.1-@version{}").

% TODO: Replace 'credits' by 'editor'? (JFMC)
% TODO: In this case, the people here are also the authors
:- doc(credits, "@bf{Edited by:}").
:- doc(credits, "Manuel Hermenegildo").
:- doc(credits, "Jose F. Morales").

% :- include(core_docsrc(common/'ClipAddress')).

:- doc(copyright, "Copyright @copyright{} 1996-2018 Manuel
Hermenegildo and Jos@'{e} Francisco Morales.

@include{FreeDocLicense.lpdoc}
").

:- doc(summary, "@include{Summary.lpdoc}").

:- doc(module, "@include{Intro.lpdoc}").

% ===========================================================================

:- doc(version_maintenance,dir('../../Manifest')).
:- doc(module, "@include{../../CHANGELOG.md}").


