:- module(_, [], [doccfg]).

%! \title  Default configuration file for running LPdoc
%  \author The Ciao Development Team
%
%  \module This is a default configuration file for running
%    \apl{lpdoc}. It is typically used as default configuration when
%    generating documentation for single modules, if the user has not
%    provided such a file. The defaults listed are typically
%    suggestions. These settings should be changed to suit your
%    application.

:- doc(bug, "Definitions that are overriden by the emacs mode must fit
   in one line. Do not use Emacs but LPdoc to generate this file.").

filepath := '/path/to/doc/sources'|'/other/path/to/doc/sources'.

doc_structure := 'main_module'.

commonopts := no_patches. % no_bugs|no_patches
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

% TODO: enable by default?
syntax_highlight := no.

