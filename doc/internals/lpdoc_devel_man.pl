:- module(lpdoc_devel_man, [], [assertions]).

:- doc(filetype, application).

:- doc(title, "The LPdoc Internals Manual").

:- doc(logo, 'lpdoc-logo-128').

:- doc(subtitle_extra, "INTERNALS MANUAL").
:- doc(subtitle_extra, "@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra, "@href{https://ciao-lang.org/}").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").
:- doc(subtitle_extra, "Technical Report CLIP 5/97.1-@version{}").

% TODO: Replace 'credits' by 'editor'? (JFMC)
% TODO: In this case, the people here are also the authors
:- doc(credits, "@bf{Edited by:}").
:- doc(credits, "Manuel Hermenegildo").
:- doc(credits, "Jose F. Morales").

% :- include(ciao_docsrc(common/'ClipAddress')).

:- doc(copyright, "Copyright @copyright{} 1996-2018 Manuel
Hermenegildo and Jos@'{e} Francisco Morales.

@include{DocCopyright.lpdoc}
").

:- doc(summary, "This is the LPdoc @concept{internals manual},
   providing information on the different internal parts of
   @apl{lpdoc} are their connections, which can be useful if new
   capabilities need to be added to the system or its libraries are
   used for other purposes.").

:- doc(module, "This is the LPdoc @concept{internals manual},
   providing information on the different internal parts of
   @apl{lpdoc} are their connections, which can be useful if new
   capabilities need to be added to the system or its libraries are
   used for other purposes.").

% ===========================================================================

:- doc(version_maintenance,dir('../../Manifest')).
%:- include('../CHANGELOG').

