:- module(autodoc_index, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Indexing commands (definition and formatting)").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module defines index commands and formatting.").

% TODO: improve the documentation

:- use_module(library(dict)).
:- use_module(library(lists), [member/2, reverse/2, append/3]).
:- use_module(library(aggregates), [findall/3]).

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_structure)).
% The database of references
:- use_module(lpdoc(autodoc_refsdb)).

%% ---------------------------------------------------------------------------

% TODO: Is it better to define @var{Comment} as a docstring or a doctree?
% TODO: are info Index names restricted to two characters?

:- export(typeindex/5).
:- pred typeindex(IdxName,IndexId,IType,Name,Comment) 
    => atom * atm * string * string * doctree
   # "Define properties of the index @var{IdxName}. @var{IndexId} is
      the index ID for texinfo. @var{IType} is the type of index; an
      empty string means normal. @var{Name} is the title of the index
      in the documentation. @var{Comment} is a comment to include
      before the index.".

%%%% Should not be empty...?
% TODO: make typeindex data so that the user can define new indices?
%:- data(typeindex/5).

typeindex(lib,    'li',"code","Library/Module Index",  string_esc("")). % TODO: split in module and package indices?
typeindex(apl,    'ap',"code","Application Index",                string_esc("")).
typeindex(pred,   'pd',"code","Predicate Index",string_esc("")).
%% typeindex(func,   'fu',"code","Function Index", string_esc("")).
typeindex(prop,   'pr',"code","Property Index",        string_esc("")).
typeindex(regtype,'te',"code","Regular Type Index",    string_esc("")).
typeindex(decl,   'de',"code","Declaration Index",     string_esc("")).
typeindex(op,     'op',"code","Operator Index",        string_esc("")).
typeindex(modedef,'mo',"code","Mode Index",            string_esc("")).
typeindex(file,   'fi',"code","File/Directory Index",             string_esc("")).
typeindex(concept,'co',""    ,"Concept Index",         string_esc("")).
typeindex(author, 'au',"" ,"Author Index",                     string_esc("")).
typeindex(cite,   'bc',"" ,"Citation Index",          string_esc("")).
%% Some versions of makeinfo get confused by this one (core dump!)
typeindex(global, 'gl',"code","Global Index",
[string_esc("This is a global index containing pointers to places
 where concepts, predicates, modes, properties, types, applications,
 authors, etc., are referred to in the text of the document.")
% ,string_esc("Note that due to limitations of the "),
% tt(string_esc("info")),
% string_esc(" format unfortunately only the first reference will appear in
% online versions of the document.")
]).

% TODO: compare with autodoc:assrt_type_text/4
def_text(lib,    "library").
def_text(apl,    "application").
%
def_text(pred,    "predicate").
def_text(compat,  "predicate").
def_text(calls,   "predicate").
def_text(success, "predicate").
def_text(comp,    "predicate").
%% def_text(func,    "FUNCTION").
def_text(prop,    "property").
def_text(regtype, "regular type").
def_text(decl,    "declaration").
def_text(modedef, "instantiation mode").
def_text(author,  "author").
def_text(entry,   "entry point").

% concepttype(index).
% concepttype(cindex).
% concepttype(concept).

:- export(idx_get_indices/3).
% Note: indices change depending on the mode of indexing (defining vs. using)
idx_get_indices(use_noidx, _Cmd, Indices) :- !,
    % use not indexed
    Indices = [].
idx_get_indices(Mode, Cmd, Indices) :-
    Indices = [global|Indices0],
    ( index_cmd(Cmd, Index, AlwaysDef, _) ->
        ( AlwaysDef = yes -> % any ocurrence acts as a definition
            Indices0 = [Index]
        ; Mode = def -> Indices0 = [Index]
        ; Indices0 = []
        )
    ; Indices0 = []
    ).

% index_cmd(Cmd, Index, Autodef, Style)
%% Concept definition index entry
index_cmd(index, concept, yes, em) :- !.
%% Concept definition index entry (NOT including the body in-line)
index_cmd(cindex, concept, yes, none) :- !.
%% Reference to concept (NOT emphasized, goes only to global)
%% Concepts should appear only once in the concept index
index_cmd(concept, concept, no, normal) :- !.
%% Authors references
index_cmd(author, author, no, normal) :- !.
%% Citation references
index_cmd(cite, cite, no, normal) :- !.
%% Predicate/Application/Property/Type/Operator/Library/File/etc. references
index_cmd(Type, Type, no, tt) :- codetype(Type), !.

:- export(is_index_cmd/1).
is_index_cmd(index) :- !.
is_index_cmd(cindex) :- !.
is_index_cmd(concept) :- !.
is_index_cmd(author) :- !.
is_index_cmd(cite) :- !.
is_index_cmd(X) :- codetype(X), !.

:- export(codetype/1).
% Indexing commands for several kinds of code
% TODO: Document each of them
codetype(lib).
codetype(apl).
codetype(pred).
%% codetype(func).
codetype(prop).
codetype(regtype).
codetype(decl).
codetype(op).
codetype(modedef).
codetype(file).
codetype(code). % TODO: guess the code type?

% ---------------------------------------------------------------------------

:- export(normalize_index_cmd/3).
% Obtain a normalized index command for the given one
normalize_index_cmd(cite, Ref, Token) :- !,
    split_cites(Ref, Refs),
    Token = idx_cite(~get_cite_items(Refs)).
normalize_index_cmd(Cmd, Body, Token) :-
    Token = idx_env(use, Cmd, localnum_label(_), Body, Body).

get_cite_items([], []).
get_cite_items([Ref|Refs], [cite_item(localnum_label(_), Ref)|Cs]) :-
    get_cite_items(Refs, Cs).

:- pred split_cites(Ref, Refs) 
# "Split citation text @var{Ref} intro individual references in
  @var{Refs}".
split_cites(Ref, Refs) :-
    remove_spaces(Ref, Ref1),
    comma_split(Ref1, Refs).

% Split comma-separated string list of strings
comma_split([], []) :- !.
comma_split(S0, [R|Rs]) :-
    comma_split_(S0, S1, R),
    comma_split(S1, Rs).

comma_split_([], [], []).
comma_split_([X|Xs], Zs, R) :-
    ( X = 0', -> Zs = Xs, R = []
    ; R = [X|R0], comma_split_(Xs, Zs, R0)
    ).

remove_spaces([],     []).
remove_spaces([32|R], RC) :- !,
    remove_spaces(R, RC).
remove_spaces([A|R], [A|RC]) :-
    remove_spaces(R, RC).

% ---------------------------------------------------------------------------

:- export(fmt_idx_env/7).
% Format a link to the index:
%   Ref: reference to the indexed element
%   IdxLabel: label of this link (for back-references)
fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R) :-
    index_cmd(Type, _, _, Style),
    ( doctree_is_empty(Body) -> R0 = []
    ; Style = none -> R0 = []
    ; Style = normal -> R0 = [Body]
    ; Style = tt -> R0 = [tt(Body)]
    ; Style = em -> R0 = [em(Body)]
    ),
    doctree_to_rawtext(Ref, DocSt, RefLab),
    % 'def' links go to the uses, 'use' or 'use_noidx' go to definition
    ( Mode = def ->
        OutLink = ~get_use_outlink(Type, RefLab, DocSt)
    ; OutLink = ~get_def_outlink(Type, RefLab, DocSt)
    ),
    %
    ( docst_backend(DocSt, texinfo) ->
        % TODO: I could use Ref, but accents break texinfo.tex
        doctree_to_rawtext(Ref, DocSt, Ref1),
        Ref2 = raw(Ref1)
    ; Ref2 = Ref
    ),
    %
    R = idx_anchor(Mode, Type, IdxLabel, Ref2, OutLink, R0).

% ---------------------------------------------------------------------------

:- export(get_use_outlink/4).
% Outlink to search page (shows other uses)
get_use_outlink(_Type, Ref, _DocSt) := OutLink :-
    ( IdxBase = ~get_subbase(~get_mainmod, 'search') ->
        OutLink = link_to(IdxBase, local_label(Ref))
    ; % TODO: warning?
      OutLink = no_link
    ).

% Outlink to definition (if not found, go to index)
get_def_outlink(Type, Ref, DocSt) := OutLink :-
    ( Type = cite -> % (special case)
        RefsBase = ~get_subbase(~get_mainmod, 'refs'),
        Base = RefsBase,
        IdxLabel = local_label(Ref),
        OutLink = link_to(Base, IdxLabel)
    ; Mode = def,
      % TODO: no indexing! slow!
      docst_gdata_query(DocSt, Base, idx(Mode, TypeD, IdxLabel, Ref)),
      equiv_idxtype(Type, TypeD) ->
        OutLink = link_to(Base, IdxLabel)
    ; % display(user_error, no_main_def(Type, Ref)), nl(user_error), % Warning?
      OutLink = ~get_use_outlink(Type, Ref, DocSt)
    ).

% Relax idxtype comparison
% TODO: it could check for inclusion
equiv_idxtype(X, Y) :- codetype(X), codetype(Y), !.
equiv_idxtype(X, X).

% ---------------------------------------------------------------------------

:- export(fmt_index/3).
fmt_index(IdxName, DocSt, R) :-
    sort_index_entries(IdxName, DocSt, DicDic),
    flatten_dic(DicDic, Groups, []),
    R = ~fmt_index_groups(Groups).

fmt_index_groups([], []).
fmt_index_groups([(G,Dic)|Gs], R) :-
    flatten_dic(Dic, KVs, []),
    index_links(KVs, Ls),
    ( symbol_norm(G) -> Header = "Symbols" ; Header = [G] ),
    R = [subsection_title(string_esc(Header)), twocolumns(itemize_env(plain, Ls))|R0],
    fmt_index_groups(Gs, R0).

% Normalized initial characters for indices
norm_alpha(X, Y) :- X >= 0'a, X =< 0'z, !, Y is X + 0'A - 0'a.
norm_alpha(X, Y) :- X >= 0'A, X =< 0'Z, !, Y = X.

symbol_norm(0). % (so that it appears the first when sorted)

% Sort index entries in groups of groups
sort_index_entries(IdxName, DocSt, Dic) :-
    Dic0 = _, % empty dictionary
    findall(idx_e(Ref,Text,IdxB), 
            query_index_entries(IdxName, DocSt, Ref, Text, IdxB),
            Es),
    sort_index_entries_(Es, Dic0, Dic).

% Obtain a dictionary of dictionaries of pairs (UseBs, DefBs)
% TODO: Text must be unique for each Ref (otherwise some Ref may be lost)
% TODO: Ref/Text is in rawtext; be careful since it may contain back-end specific escapes (except accents,
%   that are currently ignored)
sort_index_entries_([], D, D).
sort_index_entries_([idx_e(Ref,Text,IdxB)|Es], D0, D) :-
    ( Text = [G0|_], norm_alpha(G0, G) ->
        true
    ; symbol_norm(G)
    ),
    dic_lookup(D0, G, Dic0),
    ( dic_get(Dic0, Text, P0) ->
        true
    ; P0 = idx_v(Ref,[],[])
    ),
    P = ~idx_v_add(P0, IdxB),
    dic_replace(Dic0, Text, P, Dic1),
    dic_replace(D0, G, Dic1, D1),
    sort_index_entries_(Es, D1, D).

% Inserts B in P0 (using ExtMode position) to obtain P
idx_v_add(P0, B, P) :-
    B = idx_b(ExtMode,_,_),
    P0 = idx_v(Ref2,DefBs, UseBs),
    ( ExtMode = def(_) ->
        P = idx_v(Ref2,[B|DefBs],UseBs)
    ; P = idx_v(Ref2,DefBs,[B|UseBs])
    ).

% Enumerates (on backtracking) all entries for index IdxName
% ((Ref,Text,IdxB) is the result)
%
% NOTE: Ref is the symbolic reference (e.g., for anchors),
%   Text is the printed name (e.g. citations in the index)

query_index_entries(IdxName, DocSt, Ref, Text, idx_b(ExtMode, Base, IdxLabel)) :-
    ( docst_gdata_query(DocSt, Base, idx(Mode, Type, IdxLabel, Ref)) % (nondet)
    ; query_cite_idx(DocSt, Base, idx(Mode, Type, IdxLabel, Ref)) % (nondet)
    ),
    % Rewrite text (if needed)
    ( Type = cite -> Text = ~pretty_cite(Ref, DocSt)
    ; Text = Ref
    ),
    %
    ( Mode = def -> ExtMode = def(Type) % extend the mode with the type
    ; ExtMode = Mode
    ),
    Ids = ~idx_get_indices(Mode, Type),
    member(IdxName, Ids).

% Definitions of bibliographical citations extracted from biblio_pairs
query_cite_idx(DocSt, Base, idx(Mode, Type, IdxLabel, Ref)) :-
    docst_mvar_lookup(DocSt, biblio_pairs, RefPairs),
    Mode = def, Type = cite,
    RefsBase = ~get_subbase(~get_mainmod, 'refs'),
    %
    member((_,Ref), RefPairs), % (nondet)
    Base = RefsBase,
    IdxLabel = local_label(Ref).

% Extract ordered (K,V) pairs from the dictionary
flatten_dic(D, Vs, Vs) :- var(D), !.
flatten_dic(dic(K,V,L,R), Vs, Vs0) :-
    flatten_dic(L, Vs, Vs2),
    Vs2 = [(K,V)|Vs1],
    flatten_dic(R, Vs1, Vs0).

index_links([], []).
index_links([(K,idx_v(Ref,DefBs,UseBs))|KVs], Rs) :-
    reverse(DefBs, DefBs2),
    reverse(UseBs, UseBs2),
    % show links to definitions before links to uses
    append(DefBs2, UseBs2, Bs),
    Rs = [R|Rs0],
    index_key(K, Ref, Bs, R),
    index_links(KVs, Rs0).

% TODO: precondition: Bs is not empty
index_key(K, Ref, [B|Bs], R) :-
    % format the first entry
    % TODO: K already escaped?, using raw(K) instead of string_esc(K)
    index_key_single([raw(K), string_esc(" ")], B, local_label(Ref), R, R0),
    % format other entries
    index_key2(Bs, Rbs),
    ( Rbs = [] -> R0 = []
    ; R0 = [itemize_env(none, Rbs)] % include as itemize
    ).

index_key2([], []).
index_key2([B|Bs], [R|Rs]) :-
    index_key_single([], B, no_label, R, []),
    index_key2(Bs, Rs).

index_key_single(Pre, IdxB, Label, R, R0) :-
    IdxB = idx_b(ExtMode,Base,IdxLabel),
    Link = link_to(Base,IdxLabel),
    modname_nodoc(Base, Base_ND), % TODO: sure?
    atom_codes(Base_ND, BaseC),
    ( ExtMode = def(Type) ->
        ( def_text(Type, TypeC) -> true ; atom_codes(Type, TypeC) ),
        ( Type = cite -> % (hide base)
            Msg = [string_esc(TypeC)]
        ; Msg = [string_esc(BaseC), string_esc(" "), string_esc(TypeC)]
        )
    ; Msg = [string_esc("in "), string_esc(BaseC)]
    ),
    Text3 = [Pre, string_esc("("), Msg, string_esc(")")],
    R = [item(""), simple_link(default, Label, Link, Text3)|R0]. % TODO: use item_env
