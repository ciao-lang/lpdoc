:- package(doccfg).

% Package to define configuration settings for an LPdoc manual 
:- use_package(fsyntax).
:- use_package(assertions).
:- use_package(regtypes).
%:- use_package(doccomments).

% ---------------------------------------------------------------------------
% (the interface for LPdoc settings)

% TODO: use proper interfaces
:- doc(nodoc, '$implements'/1).
:- export('$implements'/1).
'$implements'('doccfg').

:- load_compilation_module(library(doccfg/doccfg_tr)).
:- add_sentence_trans(doccfg_tr:doccfg_sent/3, 8110).

% ----------------------------------------------------------------------------
% Base definition that every doccfg extends

:- include(library(doccfg/doccfg_defs)).


