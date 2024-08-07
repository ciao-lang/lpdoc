:- module(autodoc_doctree, [], [dcg, assertions, regtypes, basicmodes, fsyntax]). 

:- doc(author,"Manuel Hermenegildo (original version)").
:- doc(author,"Jose F. Morales").

:- doc(title,"Documentation abstract syntax tree").

:- doc(module, "This module defines the intermediate tree
   representation @regtype{doctree/1} for documentation and its
   related operations.").

% TODO: improve the documentation

:- use_module(engine(stream_basic)).
:- use_module(library(write), [write/2, writeq/2, write_term/3]).
:- use_module(library(operators)).
:- use_module(library(format)).
:- use_module(library(pathnames), [path_basename/2]).
:- use_module(library(lists), [member/2]).

% Local libraries
:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_parse), [spaces_or_tabs/2]).
:- use_module(lpdoc(autodoc_refsdb)).
:- use_module(lpdoc(autodoc_index)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(comments), [version_descriptor/1, docstring/1,
    stringcommand/1]).
:- use_module(lpdoc(autodoc_messages)).

% The backends
:- use_module(lpdoc(autodoc_texinfo), []).
:- use_module(lpdoc(autodoc_man),     []).
:- use_module(lpdoc(autodoc_html),    []).
:- use_module(lpdoc(autodoc_nil),     []).

% ---------------------------------------------------------------------------
% TODO: collect all interfaces

% (interface for doc_module)
:- multifile doc_cmd_type/1.
:- multifile doc_cmd_rw/2.

% ===========================================================================

:- doc(section, "Definition of LPdoc Commands").

% We provide types for each lpdoc command argument, as follows:
%
%    +------+--------------------+-------------------------------------+
%    | Name | Parsed (ciao) type | Notes                               |
%    +------+--------------------+-------------------------------------+ 
%    | d    | doctree            | Parsable, recursively rewritable    |
%    | s    | string             | Parsable                            |
%    | p    | term/term          | Parsable                            |
%    | t    | term               | Not parsable, for internal commands |
%    | td   | term               | A doctree for internal commands     |
%    +------+--------------------+-------------------------------------+
%
% This type information is used to parse a docstring into a doctree
% and to do recursive rewriting of commands.

% Valid lpdoc user commands 
:- export(cmd_type/1).
cmd_type(comment(s)) :- !.
cmd_type(include(s)) :- !.
cmd_type(includeverbatim(s)) :- !. % TODO: deprecate (at least direct use)
cmd_type(includecode(s)) :- !.
cmd_type(includecode(s,s)) :- !.
cmd_type(includefact(p)) :- !.
cmd_type(includedef(p)) :- !.
cmd_type(codeblock(s,s)) :- !. % TODO: internal for markdown
cmd_type(cite(s)) :- !.
% the following are translated during parsing to idx_env
cmd_type(index(d)) :- !.
cmd_type(cindex(d)) :- !.
cmd_type(concept(d)) :- !.
cmd_type(file(d)) :- !.
cmd_type(Command) :-
    Command =.. [Type, X], codetype(Type), !,
    X = d.
% translated during parsing to section_env
cmd_type(section(d)) :- !.
cmd_type(subsection(d)) :- !.
cmd_type(subsubsection(d)) :- !.
%
cmd_type(sp(s)) :- !.
cmd_type(p(s)) :- !.
cmd_type(noindent(s)) :- !.
cmd_type(math(s)) :- !. % TODO: Allow $...$, \(...\) notation too?
% cmd_type(displaymath(t)) :- !. (done in a 'env') % TODO: Allow $$...$$, \[...\] notation too?
cmd_type(defmathcmd(s,s)) :- !.
cmd_type(defmathcmd(s,s,s)) :- !.
cmd_type(begin(s)) :- !.
cmd_type(end(s)) :- !.
% TODO: use item_env/2 or desc_env/2 (like item_env/2, but for descriptions)
cmd_type(item(d)) :- !.
cmd_type('}') :- !.
cmd_type('{') :- !.
cmd_type('@') :- !.
cmd_type('\\') :- !.
cmd_type(today(s)) :- !.
cmd_type(version(s)) :- !.
cmd_type(hfill(s)) :- !.
cmd_type(iso(s)) :- !.
cmd_type('`'(s)) :- !.
cmd_type(''''(s)) :- !.
cmd_type(^'^'(s)) :- !. % NOTE: Escaped ^ due to fsyntax!
cmd_type('..'(s)) :- !.
cmd_type('"'(s)) :- !.
cmd_type(^'~'(s)) :- !. % NOTE: Escaped ~ due to fsyntax!
cmd_type('='(s)) :- !.
cmd_type(href(s)) :- !.
cmd_type(href(s, d)) :- !.
cmd_type(email(d)) :- !.
cmd_type(email(d, d)) :- !.
cmd_type(author(d)) :- !.
cmd_type(image(s)) :- !.
cmd_type(image(s, s, s)) :- !.
%
cmd_type(footnote(d)) :- !.
cmd_type(bf(d)) :- !.
cmd_type(em(d)) :- !.
cmd_type(tt(d)) :- !.
cmd_type(key(d)) :- !.
cmd_type(var(d)) :- !.
%% Refs to nodes / sections / chapter --some chars not allowed.
cmd_type(ref(d)) :- !.
%% Accents, etc.
cmd_type('.'(s)) :- !.
cmd_type('u'(s)) :- !.
cmd_type('v'(s)) :- !.
cmd_type('H'(s)) :- !.
cmd_type('t'(s)) :- !.
cmd_type('c'(s)) :- !.
cmd_type('d'(s)) :- !.
cmd_type('b'(s)) :- !.
cmd_type('oe'(s)) :- !.
cmd_type('OE'(s)) :- !.
cmd_type('ae'(s)) :- !.
cmd_type('AE'(s)) :- !.
cmd_type('aa'(s)) :- !.
cmd_type('AA'(s)) :- !.
cmd_type('o'(s)) :- !.
cmd_type('O'(s)) :- !.
cmd_type('l'(s)) :- !.
cmd_type('L'(s)) :- !.
cmd_type('ss'(s)) :- !.
cmd_type('?'(s)) :- !.
cmd_type('!'(s)) :- !.
cmd_type('i'(s)) :- !.
cmd_type('j'(s)) :- !.
cmd_type(copyright(s)) :- !.
cmd_type(bullet(s)) :- !.
cmd_type(result(s)) :- !.
cmd_type(html_template(s)) :- !. % TODO: move to doc_module?
cmd_type(tmplvar(s)) :- !. % TODO: move to doc_module?
% (for plain .md .lpdoc filetype)
cmd_type(title(d)) :- !.
%
% ** Semi-private commamnds **
%  Those are required just parsing bibrefs resolved with
%  docstring.bst, but should not be written by the user.
cmd_type(bibitem(s,s)) :- !.
cmd_type(newblock(s)) :- !.
%
% ** Extensible commands ** (from doc_module)
cmd_type(Cmd) :- doc_cmd_type(Cmd).

% TODO: Refine those commands 
% Internal commands (cannot be parsed from any docstring, may depend
% on the backend)

% ** Some internal commands **
icmd_type(if(t, d)). % Conditional environments
icmd_type(env_(t, d)). % Environments (from parsing)
% ** Some universal raw commands **
icmd_type(nop). % nothing
icmd_type(string_esc(t)). % a normal string (to be escaped)
icmd_type(string_verb(t)). % a verbatim string (to be escaped)
icmd_type(raw(t)). % raw text (no processing at all)
icmd_type(raw_string(t)). % raw string (already escaped)
icmd_type(raw_nl). % a raw new line
icmd_type(raw_fc). % ensure that text is written at the column 0
% a new line that eats the blanks following in the next line
%   e.g. "foo\n  bar" ==> "foo  \nbar"
icmd_type(raw_nleb).
% ** Commands for 'texinfo' **
icmd_type(infocmd(t)). % one-liner info command (no arguments)
icmd_type(infocmd(t,d)). % one-liner info command
icmd_type(infoenv(t,d)). % info enviroment (no arguments)
icmd_type(infoenv(t,d,d)). % info environment
% like infoenv/2, but only shows ups in the tex output of texinfo
% (equivalent to [d] in the info output)
icmd_type(infoenv_onlytex(t,d)).
% ** Commands for maths **
icmd_type(mathenv(t)).
icmd_type(mathenv(t,t)).
icmd_type(defmathcmd_(s,s,s)).
% ** Commands for 'html' **
icmd_type(htmlenv(t,d)). % html environment
icmd_type(htmlenv(t,t,d)). % html environment
icmd_type(htmlenv1(t)). % html environment
icmd_type(htmlenv1(t,t)). % html environment
icmd_type(htmlenv0(t,t)). % html environment
icmd_type(htmldecl(t)). % html decl
icmd_type(htmlcomment(t)). % html comment
% ** Commands for 'manl' **
icmd_type(man_page(td,t,t,t,td,td,td,td)). % html environment
% ** Shared internal commands **
icmd_type(section_env(t,t,td,td)).
% a link to a component (makes module-scope references available in
% the references closure)
% TODO: we are more specific than just 'references'
icmd_type(component_link(t)).
icmd_type(idx_env(t,t,t,d,d)). % index command
icmd_type(idx_cite(t)). % indexed citations (list of cite_item/2 inside, each with one label)
% dependency with a separated section (depending on the backend, it may include
% or not the contents of the separated section)
icmd_type(backend_include_component(t)).
icmd_type(hfill).
icmd_type(linebreak). % break the current line (should it be like "@p @noindent"?)
% a line formatted like subsection titles (bigger/stronger fonts)
% TODO: good idea? use real sections instead?
icmd_type(subsection_title(d)).
icmd_type(twocolumns(d)).
icmd_type(itemize_env(t,d)). % style can be 'none' (no mark), 'plain' (no mark, no left margin), 'minus' (-), 'bullet' (*), etc.
icmd_type(description_env(d)).
icmd_type(item_env(t,d)). % TODO: extract from item?
%icmd_type(desc_env(d,d)). % TODO: extract from item?
icmd_type(item_num(s)). % TODO: for enumerations (avoid, use context info)
icmd_type(cartouche(d)).
icmd_type(optional_cartouche(d)).
icmd_type(note(d)).
icmd_type(alert(d)).
icmd_type(idx_anchor(t,t,t,t,t,t)). % an anchor for entries in the index
icmd_type(end_document).
icmd_type(copyright_page(d)).
icmd_type(cover_title(d,t)). % the title and optional subtitle (second argument is list(d))
icmd_type(cover_subtitle_extra(d)).
icmd_type(authors(d)).
icmd_type(backend_comment(t)).
icmd_type(quotation(d)).
icmd_type(setpagenumber(t)).
icmd_type(show_index(t)).
icmd_type(backend_printindex(t)).
icmd_type(show_toc(t)). % show the table of contents (or part of it)
icmd_type(show_biblio).
icmd_type(simple_link(t,t,t,s)).
icmd_type(menu_link(t,s)). % (for texinfo)
icmd_type(ref_link(t,s)).
icmd_type(missing_link(s)).
icmd_type(pred_in_toc(t,t)).
% Write text aligned to the left and aligned to the right, in the same
% line. E.g. 
%  | foo/1          PREDICATE |
icmd_type(left_and_right(d,d)).
icmd_type(defpred(t,t,t,t,d,d)).
icmd_type(defassrt(t,s,s,d,d,d)).
icmd_type(assrtprops(d,d,d,d)).
icmd_type(defauthor(t,t,t)).
icmd_type(navigation_env(d,d)).
icmd_type(image_auto(s,t)).
%
% this one is internal -- params cannot still be read
icmd_type(html_template_internal(t,t)) :- !.

% Some icmd that cannot be further reduced by definition
% TODO: is this equal to doctokens?
primitive_icmd(nop).
primitive_icmd(raw(_)).
primitive_icmd(raw_nl).
primitive_icmd(raw_fc).
primitive_icmd(raw_nleb).
primitive_icmd(raw_string(_)).

% ===========================================================================

:- doc(section, "Doctree Definition and Basic Operations").

:- export(doctree/1).
:- regtype doctree/1 # "Intermediate tree representation for documentation".
% TODO: define based on cmd_type and icmd_type; it could be a kind of
%       'enriched' regular type.
doctree(_).

:- export(doctree_is_empty/1).
:- pred doctree_is_empty(+R) : doctree(R) # "Emptiness test".
doctree_is_empty(A) :- var(A), !, fail.
doctree_is_empty(nop).
doctree_is_empty([]).
doctree_is_empty([A|B]) :- doctree_is_empty(A), doctree_is_empty(B).
doctree_is_empty(raw([])).
doctree_is_empty(raw_string([])).
doctree_is_empty(string_esc([])).
doctree_is_empty(string_verb([])).

:- export(is_nonempty_doctree/1).
:- pred is_nonempty_doctree(+R) : doctree(R) # "Not empty test".
is_nonempty_doctree(X) :- \+ doctree_is_empty(X).

:- export(empty_doctree/1).
:- pred empty_doctree(-R) : doctree(R) # "Empty".
empty_doctree([]).

% ---------------------------------------------------------------------------

% TODO: There could be many more operations on doctree...

:- export(doctree_insert_end/3).
:- pred doctree_insert_end(A0, Elem, A) ::
   doctree * doctree * doctree 
# "Insert @var{Elem} in @var{A0} at the end, obtaining
   @var{A}".
doctree_insert_end(A0, Elem, A) :-
    doctree_simplify([A0, Elem], A).

:- export(doctree_insert_before_subfile_section/3).
:- pred doctree_insert_before_subfile_section(A0, Elem, A) :: 
   doctree * doctree * doctree 
# "Insert @var{Elem} in @var{A0} before the first section, obtaining
   @var{A}. Keep @var{A0} if @var{Elem} is already present.".

doctree_insert_before_subfile_section(A0, Elem, A) :-
    ensure_list(A0, A1),
    doctree_insert_before_subfile_section_(A1, no, Elem, A).

% If @var{Elem} is inserted at the end, @tt{More=no} must hold
% (otherwise the predicate fails).
doctree_insert_before_subfile_section_([], no, Elem, [Elem]) :- !.
doctree_insert_before_subfile_section_([A|As], _, Elem, [B|As]) :-
    doctree_insert_before_subfile_section_(A, yes, Elem, B), !.
doctree_insert_before_subfile_section_([A|As], _, Elem, [A|As]) :-
    Elem == A, !.
doctree_insert_before_subfile_section_([A|As], _, Elem, [Elem,A|As]) :-
    is_subfile_section_cmd(A), !.
doctree_insert_before_subfile_section_([A|As], More, Elem, [A|Bs]) :-
    doctree_insert_before_subfile_section_(As, More, Elem, Bs).

is_subfile_section_cmd(section_env(SecProps,_,_,_)) :-
    section_prop(subfile(_), SecProps).

ensure_list(A0, A) :-
    ( A0 = [] -> A = A0
    ; A0 = [_|_] -> A = A0
    ; A = [A0]
    ).

% ---------------------------------------------------------------------------

:- export(doctree_concat/3).
% TODO: improve
doctree_concat(A, B, C) :- is_list(A), is_list(B), !,
    append(A, B, C).
doctree_concat(A, B, [A,B]).

is_list([]).
is_list([_|_]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Documentation Links").

% TODO: current definition of the doctree regular type is incomplete

:- export(doclink/1).
:- regtype doclink/1 # "A link to a document label".
doclink(link_to(Base, Label)) :- term(Base), doclabel(Label).
doclink(no_link). % link to nowhere

:- export(doclabel/1).
:- regtype doclabel/1 # "An internal label".
% Note: global_label is useful for naming nodes in the 'info' backend
doclabel(no_label). % no label
doclabel(global_label(_)). % unique label for the document Base (mimic content's title)
doclabel(local_label(_)). % local labels to the document Base (mimic content's title)
doclabel(localnum_label(_)). % local labels to the document Base (unique numbering)

:- export(doclink_at/2).
doclink_at(link_to(Curr, _), Curr).

:- export(doclink_is_local/1).
% Links to local sections
doclink_is_local(link_to(_, local_label(_))).

% ---------------------------------------------------------------------------

:- export(doctokens/1).  
:- regtype doctokens/1 # "Primitive doctree subset (ready for output,
not further reducible)".
doctokens(_). % TODO: define

% ---------------------------------------------------------------------------

:- doc(subsection, "Section Properties").

% Properties of section_env:
%   level(L): section at level L
%   with_parent: section label appended with parent base
%   unnumbered: special unnumbered section
%   file_top_section: first section in the file (may require special translation)
% TODO: finish

:- use_module(library(lists), [select/3]).

:- export(section_prop/2).
section_prop(P, SecProps) :-
    member(P, SecProps), !.

:- export(section_select_prop/3).
section_select_prop(P, SecProps0, SecProps) :-
    select(P, SecProps0, SecProps), !.

% ---------------------------------------------------------------------------

:- doc(subsection, "Saving/Restoring a doctree to disk").

% TODO: I got a segmentation fault with fastrw! Do not work with single big terms

:- use_module(engine(runtime_control), [push_prolog_flag/2, pop_prolog_flag/1]). % TODO: find a better solution?

:- export(doctree_save/2).
:- pred doctree_save(+atm, +doctree).
doctree_save(RFile, R) :-
    push_prolog_flag(write_strings, on),
    open(RFile, write, ROS),
% TODO: faster, but breaks at some point
%       fast_write(ROS, R),
% TODO: use displayq/2?
    writeq(ROS, R), write(ROS, '.'),
    close(ROS),
    pop_prolog_flag(write_strings).

:- export(doctree_restore/2).
:- pred doctree_restore(+atm, ?doctree).
doctree_restore(RFile, R) :-
    open(RFile, read, ROS),
% TODO: faster, but breaks at some point
%       fast_read(ROS, R),
    read(ROS, R),
    close(ROS).

:- use_module(library(write), [writeq/2,write/2]).
:- use_module(library(read), [read/2]).
%:- use_module(library(fastrw), [fast_write/2, fast_read/2]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Other Operations").

:- export(doctree_simplify/2).
% TODO: when should I call this? early? or lazily? Indeed, I just want 
%   a simple way to work with sequences.
doctree_simplify([X|Xs], Ys) :- doctree_is_empty(X), !,
    doctree_simplify(Xs, Ys).
doctree_simplify([X|Xs], [X|Ys]) :- !,
    doctree_simplify(Xs, Ys).
doctree_simplify(X, X).

% TODO: improve documentation, generalize to replace other environments?
:- export(doctree_putvars/5).
:- pred doctree_putvars(R0, DocSt, PDict, VarDict, R)
    : ( doctree(R0), docstate(DocSt) ) => doctree(R) #
   "Traverse @var{R0} and replace each @tt{var(Name)} doctree item
    with a fresh variable @var{B}. For each replacement, the term
    @tt{B=Var} is introduced in @var{VarDict}, where @var{Var} is the
    associated value to @var{Name} in the dictionary @var{PDict}.".

doctree_putvars(R0, DocSt, PDict, VarDict, R) :-
    catch(doctree_putvars_(R0, DocSt, PDict, VarDict, [], R),
      E,
      throw(error(exception_error(E), doctree_putvars/5))).

doctree_putvars_(A, _DocSt, _PDict, Vs, Vs, A) :- ( var(A) ; primitive_icmd(A) ), !.
doctree_putvars_(A, DocSt, PDict, Vs, Vs0, B) :- ( A = [] ; A = [_|_] ), !,
    doctree_putvars_list(A, DocSt, PDict, Vs, Vs0, B).
doctree_putvars_(var([string_esc(VarName)]), _DocSt, PDict, Vs, Vs0, B) :- !,
    atom_codes(VarNameA, VarName),
    ( member(VarNameA=Var, PDict) -> true
    ; throw(error(domain_error,
                  doctree_putvars_/6-env(['VarNameA'=VarNameA,
                                          'PDict'=PDict])))
    ),
    % B is left uninstantiated
    Vs = [B=Var|Vs0].
doctree_putvars_(Command, DocSt, PDict, Vs, Vs0, NewCommand) :-
    functor(Command, Cmd, A),
    functor(BT, Cmd, A),
    ( cmd_type(BT) -> true
    ; icmd_type(BT) -> true
    ; throw(error(domain_error,
                  doctree_putvars_/6-env(['Command'=Command])))
    ),
    Command =.. [_|Xs],
    BT =.. [_|Ts],
    doctree_putvars_args(Ts, Xs, DocSt, PDict, Vs, Vs0, Ys),
    NewCommand =.. [Cmd|Ys].

doctree_putvars_args([], [], _DocSt, _PDict, Vs, Vs, []).
doctree_putvars_args([T|Ts], [X|Xs], DocSt, PDict, Vs, Vs0, [Y|Ys]) :-
    ( T = d -> doctree_putvars_(X, DocSt, PDict, Vs, Vs1, Y)
    ; Y = X, Vs = Vs1
    ),
    doctree_putvars_args(Ts, Xs, DocSt, PDict, Vs1, Vs0, Ys).

doctree_putvars_list([], _DocSt, _PDict, Vs, Vs, []).
doctree_putvars_list([X|Xs], DocSt, PDict, Vs, Vs0, [Y|Ys]) :-
    doctree_putvars_(X, DocSt, PDict, Vs, Vs1, Y),
    doctree_putvars_list(Xs, DocSt, PDict, Vs1, Vs0, Ys).

% ===========================================================================

:- doc(section, "Treatment and Translation of doctree").

% ---------------------------------------------------------------------------

:- doc(subsection, "Scanning (1st pass)").

:- export(doctree_scan_and_save_refs/2).
:- pred doctree_scan_and_save_refs(R, DocSt) : doctree * docstate
   # "Scan and save the references of the doctree".
doctree_scan_and_save_refs(R, DocSt) :-
    labgen_init(DocSt),
    doctree_scan_refs(R, DocSt),
    labgen_clean(DocSt),
    docst_mdata_save(DocSt),
    docst_mdata_clean(DocSt).

:- pred doctree_scan_refs(R, DocSt) : ( doctree(R), docstate(DocSt) ).
doctree_scan_refs(A, _DocSt) :-
    ( var(A) ; primitive_icmd(A) ), !.
doctree_scan_refs(A, DocSt) :- ( A = [] ; A = [_|_] ), !,
    doctree_scan_refs_list(A, DocSt).
doctree_scan_refs(L0, DocSt) :-
    % TODO: duplicated in rewrite_cmd
    L0 = section_env(SecProps, SectLabel, TitleR, Body),
    section_select_prop(subfile(SubName), SecProps, PrevSecProps),
    !,
    DocR = section_env([file_top_section|PrevSecProps], SectLabel, TitleR, Body),
    scan_subfile(SubName, DocR, DocSt, SubBase),
    doctree_scan_refs(component_link(SubBase), DocSt).
doctree_scan_refs(A, _DocSt) :-
    A = section_env(SecProps, _SectLabel, _Title, _Body),
    member(pragmas(Pragmas), SecProps),
    member(is_html_template, Pragmas),
    !,
    % Do not scan further, this is a template that should not appear
    % in the indices
    true.
doctree_scan_refs(A, DocSt) :-
    A = section_env(SecProps, SectLabel, Title, Body),
    !,
    ensure_fill_label(Title, DocSt, SectLabel),
    project_section_props(SecProps, SecProps2),
    docst_mdata_assertz(sect(SecProps2, SectLabel, Title), DocSt),
    doctree_scan_refs(Body, DocSt).
doctree_scan_refs(component_link(Base), DocSt) :-
    !,
    docst_mdata_assertz(refs_link(Base), DocSt).
doctree_scan_refs(idx_env(Mode, Type, IdxLabel, Ref, _Body), DocSt) :-
    !,
    add_idx_entry(Mode, Type, IdxLabel, Ref, DocSt).
doctree_scan_refs(idx_cite(Cs), DocSt) :- !,
    add_idx_cite(Cs, DocSt).
doctree_scan_refs(defpred(IdxLabel, Type, _, PN, _, Body), DocSt) :-
    !,
    % TODO: This logic is repeated in each backend
    PN = F/A, format_to_string("~w/~w", [F, A], S0),
    S = string_esc(S0),
    add_idx_entry(def, Type, IdxLabel, S, DocSt),
    %
    doctree_scan_refs(Body, DocSt).
doctree_scan_refs(defauthor(IdxLabel, Name, _Text), DocSt) :-
    !,
    add_idx_entry(def, author, IdxLabel, Name, DocSt).
doctree_scan_refs(Command, DocSt) :-
    functor(Command, Cmd, A),
    functor(BT, Cmd, A),
    ( cmd_type(BT) -> true
    ; icmd_type(BT) -> true
    ; throw(error(bad_arg(1, Command), doctree_scan_refs/2))
    ),
    Command =.. [_|Xs],
    BT =.. [_|Ts],
    doctree_scan_refs_args(Ts, Xs, DocSt).

% The section props that are globally visible in references
project_section_props([], []).
project_section_props([P|Ps0], Ps) :-
    ( projected_prop(P) -> Ps = [P|Ps1] ; Ps = Ps1 ),
    project_section_props(Ps0, Ps1).

projected_prop(level(_)).
projected_prop(is_special(_)).

doctree_scan_refs_args([], [], _DocSt).
doctree_scan_refs_args([T|Ts], [X|Xs], DocSt) :-
    ( T = d -> doctree_scan_refs(X, DocSt)
    ; true
    ),
    doctree_scan_refs_args(Ts, Xs, DocSt).

doctree_scan_refs_list([], _DocSt).
doctree_scan_refs_list([X|Xs], DocSt) :-
    doctree_scan_refs(X, DocSt),
    doctree_scan_refs_list(Xs, DocSt).

% Ensure that a label is filled
ensure_fill_label(Title, DocSt, SectLabel) :-
    ( SectLabel = global_label(Label0),
      var(Label0) ->
        doctree_to_rawtext(Title, DocSt, Label0)
    ; SectLabel = local_label(Label0),
      var(Label0) ->
        % TODO: useful to obtain a permalink? make it dependant to the backend?
        doctree_to_rawtext(Title, DocSt, Label0)
    ; SectLabel = localnum_label(Label0),
      var(Label0) ->
        labgen_get(DocSt, Label0)
    ; true
    ).

add_idx_entry(Mode, Type, IdxLabel, Key, DocSt) :-
    ensure_fill_label(Key, DocSt, IdxLabel),
    ( Mode = use_noidx ->
        % do not index this use
        % TODO: better index representation (e.g., trigram) could make this irrelevant
        true
    ; % TODO: Accents are lost here; use something richer than 'plaintext'? (e.g. unicode)
      doctree_to_rawtext(Key, DocSt, Key2),
      docst_mdata_assertz(idx(Mode, Type, IdxLabel, Key2), DocSt)
    ).

add_idx_cite([], _DocSt).
add_idx_cite([cite_item(IdxLabel,Ref)|Cs], DocSt) :-
    add_idx_entry(use, cite, IdxLabel, raw_string(Ref), DocSt),
    docst_mdata_assertz(citation(Ref), DocSt),
    add_idx_cite(Cs, DocSt).

% (first pass)
% TODO: See write_as_subfile
% Scan a part that will be rendered as a subfile
scan_subfile(SubSuffix, X, DocSt, SubName) :-
    docst_new_sub(DocSt, SubSuffix, DocSt1),        
    docst_currmod(DocSt1, SubName),
    doctree_scan_and_save_refs(X, DocSt1).

% ---------------------------------------------------------------------------

:- doc(subsection, "Translation of doctree to Text (2nd pass)").
% (using each backend)
% (requires doctree whose references has been scaned and saved)

% ---------------------------------------------------------------------------

:- export(doctree_prepare_docst_translate_and_write/3).
doctree_prepare_docst_translate_and_write(ModR, DocSt, OS) :-
    prepare_current_refs(DocSt),
    doctree_translate_and_write(ModR, DocSt, OS),
    clean_current_refs(DocSt),
    !.
doctree_prepare_docst_translate_and_write(_ModR, _DocSt, _OS) :-
    throw(error(unknown, doctree_prepare_docst_translate_and_write/3)).

% ---------------------------------------------------------------------------

:- export(doctree_to_rawtext/3).
:- pred doctree_to_rawtext(X, DocSt, Y) :: doctree * docstate * string
   # "@var{Y} is a simplified raw text representation of the @var{X}".

%   # "A reduced version of @pred{doctree_to_string_slow/3}".
% TODO: It can be implemented more easily with string output of streams.
% TODO: This is required for @section{}, unless the command argument
%       is limited to something simpler than a doc_string
% TODO: I would like to get rid of this and rely only on fmt_out
doctree_to_rawtext(X, DocSt, Ys) :-
    catch(doctree_to_rawtext_(X, DocSt, Ys, []),
          E,
          throw(error(exception_error(E), doctree_to_rawtext/3))).

doctree_to_rawtext_(X, _DocSt, "<var>"||Ys, Ys) :- var(X), !.
doctree_to_rawtext_([], _DocSt, Xs, Xs) :- !.
doctree_to_rawtext_([X|Xs], DocSt, Ys, Zs) :- !,
    doctree_to_rawtext_(X, DocSt, Ys, Ys1),
    doctree_to_rawtext_(Xs, DocSt, Ys1, Zs).
doctree_to_rawtext_(raw(Xs), _DocSt, Ys, Zs) :- !,
    append(Xs, Zs, Ys).
doctree_to_rawtext_(raw_string(Xs), _DocSt, Ys, Zs) :- !,
    append(Xs, Zs, Ys).
doctree_to_rawtext_(X, _DocSt, Ys, Zs) :- plaintext_ignore(X), !,
    arg(1, X, Arg),
    doctree_to_rawtext_(Arg, _DocSt, Ys, Zs).
% TODO: incomplete
doctree_to_rawtext_(X, DocSt, Ys, Zs) :- simple_escaped_command(X), !,
    atom_codes(X, C),
    doctree_to_rawtext_(string_esc(C), DocSt, Ys, Zs).
doctree_to_rawtext_(href(Xs), DocSt, Ys, Zs) :- !,
    doctree_to_rawtext_(raw(Xs), DocSt, Ys, Zs).
doctree_to_rawtext_(string_esc(Xs), DocSt, Ys, Zs) :- !,
    % TODO: Do not escape here
    escape_string(normal, Xs, DocSt, Xs2),
    append(Xs2, Zs, Ys).
doctree_to_rawtext_(string_verb(Xs), DocSt, Ys, Zs) :- !,
    % TODO: Do not escape here
    escape_string(verb, Xs, DocSt, Xs2),
    append(Xs2, Zs, Ys).
doctree_to_rawtext_(p(""), _DocSt, Ys, Zs) :- !, Ys = " "||Zs.
doctree_to_rawtext_(raw_nl, _DocSt, Ys, Zs) :- !,
    Ys = "\n"||Zs.
doctree_to_rawtext_(raw_fc, _DocSt, Ys, Zs) :- !,
    Ys = "\n"||Zs.
doctree_to_rawtext_(raw_nleb, _DocSt, Ys, Zs) :- !,
    Ys = "\n"||Zs.
doctree_to_rawtext_(X, _DocSt, Ys, Zs) :- accent_cmd(X), !,
    % Accented characters are ignored in 'rawtext'
    % TODO: This should be optional...
    arg(1, X, Y),
    append(Y, Zs, Ys).
doctree_to_rawtext_(X, _, _, _) :-
    throw(error(domain_error(X), doctree_to_rawtext_/4-1)).

accent_cmd('`'(_)).
accent_cmd(''''(_)).
accent_cmd(^'^'(_)). % NOTE: Escaped ^ due to fsyntax!
accent_cmd('..'(_)).
accent_cmd('"'(_)).
accent_cmd(^'~'(_)). % NOTE: Escaped ~ due to fsyntax!
accent_cmd('='(_)).

% TODO: generalize for all backends?
simple_escaped_command('@').
simple_escaped_command('\\').
simple_escaped_command('{').
simple_escaped_command('}').

plaintext_ignore(var(_)).
plaintext_ignore(bf(_)).
plaintext_ignore(tt(_)).
plaintext_ignore(em(_)).
plaintext_ignore(email(_)).

:- use_module(library(lists), [list_concat/2, append/3]).

% TODO: in 'doctree_to_rawtext' we omit some characters! (it is mandatory,
% e.g., to obtain names for indices, etc.).
%
%% % (complete but slower and badly implemented version)
%% :- export(doctree_to_string_slow/3).
%% doctree_to_string_slow(X, DocSt, String) :-
%%      telling(Old),
%%      mktemp(autodocXXXXXX, Tmp),
%%      tell(Tmp),
%%      ( doctree_translate_and_write(X, DocSt, user_output) ->
%%          Ok = yes
%%      ; Ok = no
%%      ),
%%      told,
%%      tell(Old),
%%      read_file(Tmp, String),
%%      delete_file(Tmp),
%%      Ok = yes.
%% 
%% :- use_module(library(system), [delete_file/1, mktemp/2]).
%% :- use_module(lpdoc(autodoc_aux), [read_file/2]).
%% :- use_module(library(dec10_io)).

% ---------------------------------------------------------------------------

:- use_module(library(terms), [atom_concat/2]).

% TODO: @pred{doctree_translate_and_write/3} is exported for trivial
%       infoindex generation

:- export(doctree_translate_and_write/3).
doctree_translate_and_write(X0, DocSt, OS) :-
    % rewrite all commands until we get a tree of tokens
    % output the tree of tokens
    rewrite_command(X0, DocSt, X2),
    ( doctokens_write(X2, DocSt, OS) ->
        true
    ; throw(error(wrong_arg(1,X2), doctokens_write/3))
    ).

% ---------------------------------------------------------------------------

rewrite_command(X0, DocSt, X2) :-
    ( rewrite_command_(X0, DocSt, X2) ->
        true
    ; throw(error(wrong_arg(1,X0), rewrite_command/3))
    ).
    
rewrite_command_(L, _DocSt, L) :- ( var(L) ; primitive_icmd(L) ), !.
rewrite_command_(A, DocSt, B) :- ( A = [] ; A = [_|_] ), !,
    rewrite_commands(A, DocSt, B).
% Rewrite a codeblock if not supported by the backend
rewrite_command_(codeblock(_Lang,Content), DocSt, R) :-
    docst_backend(DocSt, Backend),
    \+ Backend = html,
    !,
    % TODO: here type is not 'normal' but 'verb' (<- sure?)
    escape_string(normal, Content, DocSt, NContent0),
    ( remove_first_nl_str(NContent0, NContent1) -> NContent = NContent1
    ; NContent = NContent0
    ),
    rewrite_command_(env_('verbatim', [raw_string(NContent)]), DocSt, R).
% TODO: Hack to fix unwanted blanks after "@begin{verbatim}", solve it in a better way
rewrite_command_(env_('verbatim', Body0), DocSt, R) :-
    remove_first_nl(Body0, Body),
    !,
    rewrite_command_(env_('verbatim', Body), DocSt, R).
% TODO: Hack to switch item into item_num for enumerations, solve it
%       by introducing context.
rewrite_command_(env_('enumerate', Body0), DocSt, R) :-
    member(item(_), Body0),
    !,
    % Use internal item_num for enumerate
    item_to_item_num(Body0, DocSt, Body),
    docst_backend(DocSt, Backend),
    ( % A hack, explicit values in items is not supported by
      % texinfo, so we fake it.
      Backend = texinfo, 
      member(item_num(N), Body), \+ N = "" ->
        R2 = description_env(Body)
    ; R2 = env_('enumerate', Body)
    ),
    rewrite_command_(R2, DocSt, R).
rewrite_command_(Command, DocSt, NewCommand2) :-
    functor(Command, Cmd, A),
    functor(BT, Cmd, A),
    Command =.. [_|Xs],
    ( cmd_type(BT) -> Kind = cmd % TODO: Kind unused
    ; icmd_type(BT) -> Kind = icmd % TODO: Kind unused
    ; throw(error(wrong_arg(1,Command), rewrite_command/3))
    ),
    BT =.. [_|Ts],
    rewrite_cmd_args(Ts, Xs, DocSt, Ys),
    B1 =.. [Cmd|Ys],
    rewrite_cmd(B1, DocSt, NewCommand),
    rewrite_command_(NewCommand, DocSt, NewCommand2).

rewrite_cmd(L0, DocSt, R) :-
    L0 = section_env(SecProps, SectLabel, TitleR, Body),
    section_select_prop(subfile(SubName), SecProps, PrevSecProps),
    !,
    DocR = section_env([file_top_section|PrevSecProps], SectLabel, TitleR, Body),
    write_as_subfile(SubName, DocR, DocSt, SubBase),
    R = component_link(SubBase).
rewrite_cmd(component_link(Base), DocSt, L) :- !,
    % Note: this translation is just for the texinfo backend
    % (for the other backends, doctree_scan_refs has already made its work)
    docst_backend(DocSt, Backend),
    absfile_for_subtarget(Base, Backend, cr, F),
    % TODO: hack, remove the path since we live in the same directory
    path_basename(F, FName),
    L = backend_include_component(FName).
% ... (shared commands) ...
rewrite_cmd(version(""), DocSt,  R) :- !,
    % TODO: Allow different views of the version
    ( docst_gdata_query(DocSt, main_globalvers(GlobalVers)) ->
        version_numstr(GlobalVers, Str),
        R = string_esc(Str)
    ; R = string_esc("<version>")
    ).
rewrite_cmd(math(X), _DocSt,  R) :- !,
    fmt_to_latex(X, X1),
    R = mathenv(X1).
rewrite_cmd(env_('displaymath', X), DocSt,  R) :- !,
    doctree_to_rawtext(X, DocSt, X1),
    fmt_to_latex(X1, X2),
    R = mathenv(display, X2).
rewrite_cmd(defmathcmd(Cmd,N,Def), _DocSt,  R) :- !,
    fmt_to_latex(Cmd, Cmd1),
    fmt_to_latex(N, N1),
    fmt_to_latex(Def, Def1),
    R = defmathcmd_(Cmd1, N1, Def1).
rewrite_cmd(defmathcmd(Cmd,Def), _DocSt,  R) :- !,
    R = defmathcmd(Cmd, "0", Def).
rewrite_cmd(image(F), _DocSt, R) :- !, R = image_auto(F, []).
rewrite_cmd(image(F,X,Y), _DocSt, R) :- !, R = image_auto(F, [X,Y]).
rewrite_cmd(idx_env(Mode, Type, IdxLabel, Ref, Body), DocSt, R) :-
    !,
    fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R).
rewrite_cmd(defauthor(IdxLabel, Name, Text), DocSt, R) :- !,
    % TODO: merge with idx_env or defpred?
    Mode = def, Type = author, Ref = Name, Body = Text,
    fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R).
rewrite_cmd(show_toc(TOCKind), DocSt, R) :- !,
    R = ~fmt_toc(TOCKind, DocSt).
rewrite_cmd(show_biblio, DocSt, R) :- !,
    ( docst_mvar_get(DocSt, biblio_doctree, RefsR0) ->
        ( RefsR0 = [] ->
            % TODO: Generalize for other sections?
            R = string_esc("(this section is empty)")
        ; R = env_('description', RefsR0)
        )
    ; % No bibliography yet. This happens when a doctree is
      % written before the bibliography is resolved.
      R = string_esc("[ERROR - NO BIBLIOGRAPHY]")
    ).
rewrite_cmd(show_index(IdxName), DocSt, R) :- !,
    docst_backend(DocSt, Backend),
    ( Backend = html ->
        % format the index ourselves
        fmt_index(IdxName, DocSt, R)
    ; % let the backend print the index
      R = backend_printindex(IdxName)
    ).
rewrite_cmd(idx_cite(Cs), DocSt, R) :- !,
    fmt_idx_cite(Cs, DocSt, R).
rewrite_cmd(ref(Ref), DocSt, R) :- !,
    fmt_ref(Ref, DocSt, R).
% TODO: include here general shared internal commands
rewrite_cmd(string_esc(X), DocSt, R) :- !,
    R = raw_string(X2),
    escape_string(normal, X, DocSt, X2).
rewrite_cmd(string_verb(X), DocSt, R) :- !,
    R = raw_string(X2),
    escape_string(verb, X, DocSt, X2).
% .......... (commands defined in a doc_module) ..........
rewrite_cmd(Cmd, _DocSt, R) :-
    functor(Cmd, F, A),
    functor(Cmd2, F, A),
    doc_cmd_type(Cmd2), % (defined externally?)
    !,
    doc_cmd_rw(Cmd, R).
% .......... (backend definition of commands) ..........
rewrite_cmd(X, DocSt, L) :-
    docst_backend(DocSt, Backend),
    autodoc_rw_command_hook(Backend, DocSt, X, L).

rewrite_cmd_args([], [], _, []).
rewrite_cmd_args([T|Ts], [X|Xs], DocSt, [Y|Ys]) :-
    ( T = d -> rewrite_command(X, DocSt, Y)
    ; Y = X
    ),
    rewrite_cmd_args(Ts, Xs, DocSt, Ys).

rewrite_commands([], _DocSt, []).
rewrite_commands([X|Xs], DocSt, [Y|Ys]) :-
    rewrite_command(X, DocSt, Y),
    rewrite_commands(Xs, DocSt, Ys).

:- multifile autodoc_rw_command_hook/4. 

write_as_subfile(SubSuffix, DocR0, DocSt, SubName) :-
    docst_new_sub(DocSt, SubSuffix, DocSt1),
    ( insert_show_toc(DocR0, DocSt1, DocR) -> % TODO: move to doctree_prepare_...?
        true
    ; throw(error(wrong_arg(1,DocR0), insert_show_toc/3))
    ),
    %
    docst_backend(DocSt1, Backend),
    docst_currmod(DocSt1, SubName),
    absfile_for_subtarget(SubName, Backend, cr, SubFile),
    % TODO: close file on exception
    open(SubFile, write, SubOS),
    doctree_prepare_docst_translate_and_write(DocR, DocSt1, SubOS),
    close(SubOS).

% ---------------------------------------------------------------------------

% Replace all 'item' by 'item_num' (for use in enumerations)
% TODO: This should not be necessary if we had some context information
item_to_item_num([], _, []).
item_to_item_num([A|As], DocSt, [B|Bs]) :-
    item_to_item_num(A, DocSt, B),
    item_to_item_num(As, DocSt, Bs).
item_to_item_num(item(S0), DocSt, B) :- !,
    B = item_num(S),
    doctree_to_rawtext(S0, DocSt, S).
item_to_item_num(A, _, A).

% ---------------------------------------------------------------------------

:- export(escape_string/4).
:- pred escape_string/4 => atom * string * docstate * string

   # "Escapes needed characters in input string as needed for the
      target format.".

% Modes for escaping: 
%   normal: a normal string
%   verb: a verbatim string 

escape_string(InputType, NS, DocSt, VS) :-
    docst_backend(DocSt, Backend),
    ( autodoc_escape_string_hook(Backend, InputType, NS, DocSt, VS) ->
        true
    ; VS = NS % TODO: wrong! define escape_string for all backends
    ).

:- multifile autodoc_escape_string_hook/5.

% ---------------------------------------------------------------------------

:- doc(subsection, "Writing doctokens").

:- pred doctokens_write(X, DocSt, OS) : doctokens * docstate * stream
    # "Write doctokens to the @var{OS} stream".

% Internal state of the algorithm:
%
%   NL=nl:   The previous character was a new line
%   NL=nonl: The previous character was not a new line
%   NL=nl_eat_blank: 
%          The previous character was a new line but was not
%          written. Space after it are moved before the new line.
%          (useful for writing 'info' files)

% TODO: Refine usage of special raw_nleb, raw_fc commands
%       (e.g. not necessary for all backend)

doctokens_write(X2, DocSt, OS) :-
    doctokens_write_(X2, nonl, NL, DocSt, OS),
    ( NL = nl_eat_blank ->
        format(OS, "\n", [])
    ; true
    ).

doctokens_write_(A, NL, NL, _DocSt, _OS) :- var(A), !.
doctokens_write_(A, NL, NNL, DocSt, OS) :- ( A = [] ; A = [_|_] ), !,
    doctokens_write_list(A, NL, NNL, DocSt, OS). 
doctokens_write_(nop, NL, NL, _DocSt, _OS) :- !.
% (In nl_eat_blank state)
doctokens_write_(raw_string(" "), nl_eat_blank, nl_eat_blank, _DocSt, OS) :- !,
    format(OS, " ", []).
% (eating more than one blank was worse than expected --JF)
%doctokens_write_(raw_string(""), NL, NL, _DocSt, _OS) :- !.
%doctokens_write_(raw_string(" "||Rest), nl_eat_blank, NNL, DocSt, OS) :- !,
%       format(OS, " ", []),
%       doctokens_write_(raw_string(Rest), nonl, NNL, DocSt, OS).
doctokens_write_(A, nl_eat_blank, NL, DocSt, OS) :- !,
    format(OS, "~n", []),
    doctokens_write_(A, nl, NL, DocSt, OS).
% (Not in nl_eat_blank state)
%doctokens_write_(raw(String), _NL, nonl, _DocSt, _OS) :- var(String), !,
%       % TODO: Why? only in XML backend
%       true.
doctokens_write_(raw(String), _NL, nonl, _DocSt, OS) :- nonvar(String), !,
    format(OS, "~s", [String]).
doctokens_write_(raw_fc, nl, nl, _DocSt, _OS) :- !.
doctokens_write_(raw_fc, nonl, nl, _DocSt, OS) :- !,
    format(OS, "~n", []).
doctokens_write_(raw_nl, _, nl, _DocSt, OS) :- !,
    format(OS, "~n", []).
doctokens_write_(raw_nleb, _, nl_eat_blank, _DocSt, _OS) :- !.
doctokens_write_(raw_string(String), _NL, NL, _DocSt, OS) :- !,
    % detect if there was a newline
    ( append(_, "\n", String) ->
        NL = nl
    ; NL = nonl
    ),
    format(OS, "~s", [String]).
doctokens_write_(A, NL, _, _, _) :-
    throw(error(domain_error, doctokens_write_/5-env(['A'=A, 'NL'=NL]))).

doctokens_write_list([], NL, NL, _DocSt, _OS) :- !.
doctokens_write_list([R|Rs], NL0, NL, DocSt, OS) :-
    doctokens_write_(R, NL0, NL1, DocSt, OS),
    doctokens_write_list(Rs, NL1, NL, DocSt, OS).

%% ---------------------------------------------------------------------------

% TODO: this contains some hardwired cases, can it be improved? --JF
remove_first_nl([string_esc(" ")|Xs0], Xs) :- !,
    remove_first_nl(Xs0, Xs).
remove_first_nl([string_verb(S0)|Xs], [string_verb(S)|Xs]) :- !,
    remove_first_nl_str(S0, S).

remove_first_nl_str(" "||S0, S) :- !, remove_first_nl_str(S0, S).
remove_first_nl_str("\n"||S0, S) :- !, S = S0.

% ===========================================================================

:- doc(section, "Versions and Operations on Versions").

:- export(is_version/1).
is_version(Version) :- nonvar(Version), Version \== [].

version_format(version(V*SV+P, Y/M/D),
           V, SV, P, Y, M, D, [], [], [], []).
version_format(version(V*SV+P, Y/M/D, []),
           V, SV, P, Y, M, D, [], [], [], []).
version_format(version(V*SV+P, Y/M/D, H:N*S+Z),
           V, SV, P, Y, M, D, H,  N,  S,  Z).

:- export(version_patch/2).
version_patch(V, VPatch) :-
    version_format(V, _, _, VPatch, _, _, _, _, _, _, _).

:- export(version_date/2).
version_date(Version, Date) :-
    ( version_format(Version, _, _, _, Year, Month, Day, _, _, _, _) ->
        Date = Year/Month/Day
    ; fail
    ).

:- export(version_numstr/2).
:- pred version_numstr(Version, Str) # "Obtain the string @var{Str}
   representation of version @var{Version} (except date)".

version_numstr(Version, Str) :-
    ( Version = version(Ver*Sub+Patch, _)
    ; Version = version(Ver*Sub+Patch, _, _)
    ),
    !,
    ( Patch = 0 ->
        format_to_string("~w.~w", [Ver, Sub], Str)
    ; format_to_string("~w.~w#~w", [Ver, Sub, Patch], Str)
    ).

:- export(version_string/2).  
:- pred version_string(Version, Str) # "Obtain the string @var{Str}
   representation of version @var{Version} (including date)".

version_string(Version, Str) :-
    ( Version = version(_, Date), Time = []
    ; Version = version(_, Date, Time)
    ),
    !,
    % Format version number
    version_numstr(Version, Sv),
    % Format date/time
    ( Time = [] ->
        format_to_string("(~w)", [Date], Sd)
    ; Time = H:M*S+Z ->
        format_to_string("(~w, ~w:~w:~w ~w)", [Date, H, M, S, Z], Sd)
    ; throw(error(unknown_time(Time), version_string/2))
    ),
    list_concat([Sv, " ", Sd], Str).
version_string(Version, Str) :-
    autodoc_message(warning, "Unrecognized version format '~w'", [Version]),
    Str = "".

:- export(parse_changelog_version/3).
% "[Version] - Date" for changelog entries
parse_changelog_version(Vers) -->
    "[", parse_nat(Major), ".", parse_nat(Minor), ".", parse_nat(Patch), "]",
    spaces_or_tabs,
    "-",
    spaces_or_tabs,
    parse_nat(Year), "-", parse_nat(Month), "-", parse_nat(Day),
    spaces_or_tabs,
    !,
    { Vers = version(Major*Minor+Patch,Year/Month/Day,[]) }.

parse_nat(Num) -->
    digits(Ds),
    { number_codes(Num, Ds) }.

digits([C|Cs]) --> digit(C), !, digits(Cs).
digits([]) --> [].

digit(C) --> [C], { digit_p(C) }.

digit_p(C) :- C >= 0'0, C =< 0'9.

% ===========================================================================

:- doc(section, "Common Formatting Operations").

% ---------------------------------------------------------------------------

:- doc(subsection, "Formatting Cites and References").

% TODO: it may be different for other backends that resolve cites
%   themselves (e.g., latex)

:- pred fmt_idx_cite(Cs, DocSt, R) 
# "Add @tt{idx_env/5} for each bibliographical citation in @var{Cs}".
fmt_idx_cite(Cs, DocSt, R) :-
    R1 = ~cite_idxs(Cs, DocSt),
    R = [string_esc("["), R1, string_esc("]")].

cite_idxs([], _, []).
cite_idxs([C|Cs], DocSt, [R|Rs0]) :-
    cite_idx(C, DocSt, R),
    ( Cs = [] ->
        Rs0 = []
    ; Rs0 = [string_esc(",")|Rs1],
      cite_idxs(Cs, DocSt, Rs1)
    ).

cite_idx(cite_item(IdxLabel, Ref), DocSt, R) :-
    Text = ~pretty_cite(Ref, DocSt),
    RefR = raw_string(Ref),
    TextR = string_esc(Text),
    R = idx_env(use, cite, IdxLabel, RefR, TextR).

% ---------------------------------------------------------------------------

:- pred fmt_ref(Ref, DocSt, R) # "Process a section reference".
% (note: currently sections can only be referenced by its name)
fmt_ref(Ref0, DocSt, R) :-
    ( docst_mvar_get(DocSt, full_toc_tree, FullTree) ->
        doctree_to_rawtext(Ref0, DocSt, Ref1),
        resolve_ref(Ref1, FullTree, R)
    ; throw(error(no_full_toc_tree, fmt_ref/3))
    ).

resolve_ref(Ref, FullTree, R) :-
    % TODO: make RefPairs a dictionary so that this can be faster
    ( secttree_resolve(Ref, FullTree, Link) ->
        R = ref_link(Link, Ref)
    ; % TODO: Emit warning here?
      R = missing_link(Ref),
      autodoc_message(warning, "Could not resolve @ref{~s}~n", [Ref])
    ).

% ---------------------------------------------------------------------------

:- doc(subsection, "Formatting the Table of Contents").
% (with references to sections/subsections)
% 
% We allow different views of the full table of contents. The
% @pred{toc_kind/1} specifies the available views. The same page may
% include more than one view.

% NOTE: See texinfo documentation for menu entries (info backend)
% https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html#Writing-a-Menu
% https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html#Master-Menu-Parts

% TODO: change names
:- regtype toc_kind/1.
toc_kind := subparts      % First level of TOC subtree
      | sidebar       % Sidebar navigation
      | single(_)     % Special, single section (e.g. toc, copyright, etc.)
      | full          % Global (all except local contents)
      | navmenu(_)    % Navigation menu (for web pages)
      | global        % Global (all except local contents, hide children)
      | local         % Local contents (current page)
      | indices.      % Indices

% TODO: include the navigation buttons as views here?
fmt_toc(sidebar, DocSt, R) :- !, % (only for HTML)
    % Seach input, navigation for global, local, and special sections
    % Rs = ~fmt_search_input(DocSt),
    Rn = ~fmt_sectnav(DocSt),
    Rl = ~fmt_localsect(DocSt),
    Rc = ~fmt_customsect(DocSt),
    % (All)
    doctree_simplify([
        /*Rs,*/ Rn, Rl, Rc], R).
fmt_toc(subparts, DocSt, R) :- !,
    docst_backend(DocSt, Backend),
    show_subparts(DocSt, SubpartsInText),
    ( Backend = texinfo ->
        ( docst_currmod_is_main(DocSt) ->
            Rg = ~fmt_toc(global_and_indices, DocSt)
        ; Rg = ~fmt_toc(global, DocSt)
        ),
        doctree_simplify([Rg], R)
    ; ( SubpartsInText = yes ->
          Rg = ~fmt_toc(global, DocSt)
      ; Rg = []
      ),
      doctree_simplify([Rg], R)
    ).
fmt_toc(single(Kind), DocSt, R) :- !,
    % TODO: This is a ugly and slow hack; copy nav implementation
    ( docst_mvar_lookup(DocSt, full_toc_tree, Tree0) ->
        true
    ; throw(error(menu_not_computed, fmt_toc/3))
    ),
    % Remove the root node
    Tree0 = [toc_node(_,_,_,Tree)],
    %
    ( member(toc_node(Link,T,Props,_Subs), Tree),
      section_prop(is_special(Kind), Props) ->
        R = simple_link(default, no_label, Link, T)
    ; R = []
    ).
fmt_toc(navmenu(MenuStyle), DocSt, R) :- !,
    Tree = ~get_menu_tree(DocSt, MenuStyle),
    docst_currmod(DocSt, Name),
    R0 = ~fmt_navmenu(Tree, Name, 0, MenuStyle),
    ( MenuStyle = horizontal -> % TODO: flag to add search box
        % horizontal menu, add search link
        SLink = link_to(url('/ciao/build/doc/ciao.html/ciaosearch.html'), no_label),
        SearchR = item_env(unselmenu, simple_link('lpdoc-searchmenu', no_label, SLink, raw("&#x1F50D;"))),
        R1 = ~append(R0, [SearchR])
    ; R1 = R0
    ),
    ( \+ doctree_is_empty(R1) ->
        ( MenuStyle = horizontal ->
            R = [itemize_env(horizontal_menu, R1)]
        ; % MenuStyle = vertical
          R = [itemize_env(menu, R1)]
        )
    ; R = []
    ).
fmt_toc(global_and_indices, DocSt, R) :- !,
    % TODO: ad-hoc for texinfo backend (simplify)
    Rg = ~fmt_toc_tree(global, DocSt),
    Ri = ~fmt_toc_tree(indices, DocSt),
    ( \+ doctree_is_empty(Ri) ->
        doctree_simplify([Rg, raw_nl, raw("Indexes"), raw_nl, raw_nl, Ri], R0)
    ; R0 = Rg
    ),
    ( \+ doctree_is_empty(R0) ->
        R = [infoenv("menu", R0)]
    ; R = []
    ).
fmt_toc(TOCKind, DocSt, R) :-
    R0 = ~fmt_toc_tree(TOCKind, DocSt),
    R = ~fmt_toc_env(R0, DocSt, TOCKind).

get_menu_tree(DocSt, MenuStyle) := Tree :-
    ( docst_mvar_lookup(DocSt, full_toc_tree, Tree0) ->
        true
    ; throw(error(menu_not_computed, fmt_toc/3))
    ),
    % Remove the root node
    Tree0 = [toc_node(_,_,_,Tree1)],
    ( custom_html_layout, MenuStyle = vertical -> % TODO: ad-hoc, fix
        % vertical menu, put root node as a separate link
        Tree0 = [toc_node(TLink,_,TProps,_)],
        Tree = [toc_node(TLink,string_esc("Home"),TProps,[])|Tree1]
    ; Tree = Tree1
    ).

show_subparts(DocSt, InText) :-
    docst_filetype(DocSt, FileType),
    ( subparts_in_text(FileType) ->
        InText = yes
    ; InText = no
    ).

% TODO: show always in text?
subparts_in_text(application).
% subparts_in_text(documentation). % No, these files are bigger!
subparts_in_text(part).

% Enclose the result of fmt_toc_tree in a proper environment
% (this separation is useful for texinfo menus)
fmt_toc_env(R0, _DocSt, _TOCKind, R) :-
    doctree_is_empty(R0),
    !,
    R = [].
fmt_toc_env(R0, DocSt, _TOCKind, R) :-
    docst_backend(DocSt, texinfo),
    !,
    R = [infoenv("menu", R0)].
fmt_toc_env(R0, DocSt, TOCKind, R) :-
    docst_filetype(DocSt, FileType),
    toc_title(TOCKind, FileType, Title),
    ( doctree_is_empty(Title) -> R = R1 % no title
    ; R = [subsection_title(Title)|R1]
    ),
    R1 = [itemize_env(bullet, R0)].

% ---------------------------------------------------------------------------
% Format as a tree the table of contents given by @var{TOCKind}.

fmt_toc_tree(TOCKind, DocSt) := R :-
    docst_backend(DocSt, Backend),
    R = ~fmt_toc_tree_(~get_toc_tree(TOCKind, DocSt), Backend, TOCKind).

get_toc_tree(TOCKind, DocSt, Tree) :-
    toc_source(TOCKind, Source),
    ( docst_mvar_lookup(DocSt, Source, Tree0) ->
        true
    ; throw(error(menu_not_computed, fmt_toc/3))
    ),
    ( Source = full_toc_tree -> 
        % Remove the root node
        Tree0 = [toc_node(_,_,_,Tree)]
    ; Tree = Tree0
    ).

% Format the node tree.
fmt_toc_tree_([], _Backend, _) := [].
fmt_toc_tree_([toc_node(Link,T,Props,Subs)|Ss], Backend, TOCKind) := Rs :-
    % TODO: Use Props to implement views for indices, etc.
    ( toc_link_filter(TOCKind, Props, Link, Recursive) ->
        Rs = [R|Rs0],
        ( Recursive = yes ->
            SubRs = ~fmt_toc_tree_(Subs, Backend, TOCKind)
        ; SubRs = []
        ),
        R = ~fmt_toc_link(Backend, default, Link, T, SubRs)
    ; Rs = Rs0
    ),
    % Continue with the rest of nodes
    Rs0 = ~fmt_toc_tree_(Ss, Backend, TOCKind).

fmt_toc_link(texinfo, _Style, Link, Title, SubRs) := R :- !,
    R = [menu_link(Link, Title)|R0],
    ( SubRs = [] ->
        R0 = []
    ; throw(nested_texinfo_toc_not_implemented) % (not allowed, need flatten)
    ).
fmt_toc_link(_, Style, Link, Title, SubRs) := R :- !,
    html_menustyle(Style, HtmlStyle), % TODO: ugly (should not be here)
    R = [item_env(Style, simple_link(HtmlStyle, no_label, Link, Title))|R0], % TODO: to item_env?
    ( SubRs = [] ->
        R0 = []
    ; menustyle(Style) ->
        R0 = [itemize_env(menu, SubRs)]
    ; R0 = [itemize_env(bullet, SubRs)]
    ).

html_menustyle(default, default). % (no style)
html_menustyle(selmenu, 'lpdoc-selmenu').
html_menustyle(unselmenu, 'lpdoc-unselmenu').
html_menustyle(phonymenu, 'lpdoc-phonymenu').

menustyle(selmenu).
menustyle(unselmenu).
menustyle(phonymenu).

% ---------------------------------------------------------------------------
% Format search input box/link (for HTML)

% TODO: right now it is just a magnifying glass; no input box yet

% fmt_search_input(_DocSt) := Rs :-
%       Rs = [simple_link(default, no_label, ~search_link, string_esc("Search...")),
%             linebreak].

% Link to the search
search_link(R) :-
    % TODO: 'no_label' is wrong (it works because it is ignored in HTML backend)
    R = link_to(~get_subbase(~get_mainmod, 'search'), no_label).

% ---------------------------------------------------------------------------
% Format section navigation (arrows, path, children) for sidebar (only HTML)

fmt_sectnav(DocSt) := R :-
    % TOC and navigation arrows
    Rt = simple_link(default, no_label, ~fulltoc_link, string_esc("TOC")),
    UpPrevNextR = ~fmt_navlinks(DocSt),
    % Children items
    RnItems = ~fmt_toc_tree(global, DocSt), % TODO: rename 'global' by children?
    ( doctree_is_empty(RnItems) -> Rn = []
    ; Rn = [raw(" &#9662;"), itemize_env(bullet, RnItems)]
    ),
    % Section navigation (path and children)
    SectPathR = ~fmt_navpath(Rn, DocSt),
    R = [navigation_env(Rt, UpPrevNextR),
         htmlenv(hr, []),
         itemize_env(sectpath, SectPathR)].

% Link to the fulltoc
fulltoc_link(R) :-
    % TODO: 'no_label' is wrong (it works because it is ignored in HTML backend)
    R = link_to(~get_subbase(~get_mainmod, 'fulltoc'), no_label).

% Format the navigation path
% (links to the parent, the previous and the next nodes)
fmt_navpath(Rn, DocSt) := PathR :-
    ( docst_mvar_get(DocSt, nav, Nav) ->
        true
    ; throw(error(no_navigation, fmt_nav/3))
    ),
    Nav = nav(Path, _Top, _Up, _Prev, _Next),
    navpath_links(Path, Path2),
    nav_items(Path2, raw(" &raquo;<br/> "), Rn, PathR0),
    ( PathR0 = [] -> PathR = raw("&nbsp;") 
    ; PathR = PathR0
    ).

% :- regtype step := step(..., ...).
% :- regtype path := ~list(step).
% :- pred pathlink/2 :: path * list(doctree).
navpath_links([], []).
navpath_links([step(Link, Title)], [R]) :- !,
    % (last in bold)
    R = [simple_link(default, no_label, Link, bf(Title))].
navpath_links([step(Link, Title)|Path], [R|Rs]) :-
    R = [simple_link(default, no_label, Link, Title)],
    navpath_links(Path, Rs).

% nav_items(As, Suff, SuffLast, Bs): Add items for each element in As,
%   adding Suff for all the items except the last one, and SuffLast
%   for the last one.
nav_items([], _, _, []) :- !.
nav_items([A], _, SuffLast, [item_env(default, [A,SuffLast])]) :- !.
nav_items([A|As], Suff, SuffLast, [item_env(default, [A,Suff])|Bs]) :-
    nav_items(As, Suff, SuffLast, Bs).

% ---------------------------------------------------------------------------
% Format the navigation links (for HTML)
% (links to the parent, the previous and the next nodes)

fmt_navlinks(DocSt) := UpPrevNextR :-
    ( docst_mvar_get(DocSt, nav, Nav) ->
        true
    ; throw(error(no_navigation, fmt_nav/3))
    ),
    Nav = nav(_Path, _Top, Up, Prev, Next),
    % % Triangles as arrows (it does not look nice in some devices)
    % UpUnicode = raw("&#x25B2;"),
    % LeftUnicode = raw("&#x25C4;"),
    % RightUnicode = raw("&#x25BA;"),
    %
    % Magnifying glass (for search)
    MagUnicode = raw("&#x1F50D;"), % TODO: replace by a proper search input box?
    navlink(~search_link, MagUnicode, SearchR),
    % Arrows (it looks nicer in most devices)
    LeftUnicode = raw("&#x2190;"),
    RightUnicode = raw("&#x2192;"),
    ( Up = no_link ->
        UpUnicode = raw("&#x2302;"), % home unicode
        Up2 = link_to(url('/'), no_label) % Link to dir % TODO: make it optional?
    ; UpUnicode = raw("&#x2191;"),
      Up2 = Up
    ),
    navlink(Up2, UpUnicode, UpR),
    navlink(Prev, LeftUnicode, PrevR),
    navlink(Next, RightUnicode, NextR),
    UpPrevNextR = [UpR, PrevR, NextR, SearchR].

navlink(Link, Text, R) :-
    Style = ~navlink_style(Link),
    R = simple_link(Style, no_label, Link, Text).

navlink_style(no_link) := Style :- !, Style = 'lpdoc-navbutton-disabled'. % deactivated link
navlink_style(_) := 'lpdoc-navbutton'.

% ---------------------------------------------------------------------------
% Format the node tree as a navigation menu (for web pages, only HTML)

fmt_navmenu([], _, _Depth, _) := [].
fmt_navmenu([toc_node(Link,T,Props,Subs)|Ss], Name, Depth, MenuStyle) := Rs :-
    ( % TODO: same condition than in 'full' toc?
      \+ doclink_is_local(Link),
      \+ section_prop(is_special(_), Props) ->
        Rs = [R|Rs0],
        Depth1 is Depth + 1,
        SubRs = ~fmt_navmenu(Subs, Name, Depth1, MenuStyle),
        ( Link = link_to(Name, _) -> Style = 'selmenu'
        ; Link = no_link -> Style = 'phonymenu'
        ; Style = 'unselmenu'
        ),
        ( MenuStyle = vertical, Depth = 0 -> T2 = bf(T) ; T = T2 ), % use bold for first level in vertical menu
        R = ~fmt_toc_link(html, Style, Link, T2, SubRs)
    ; Rs = Rs0
    ),
    % Continue with the rest of nodes
    Rs0 = ~fmt_navmenu(Ss, Name, Depth, MenuStyle).

% ---------------------------------------------------------------------------
% Local sections (on this page)

fmt_localsect(DocSt) := Rl :-
    ( docst_currmod_is_main(DocSt) -> % Not in cover
        Rl = []
    ; RlItems = ~fmt_toc_tree(local, DocSt),
      ( doctree_is_empty(RlItems) -> Rl = []
      ; Rl = [htmlenv(hr, []),
              em(string_esc("ON THIS PAGE")),
              itemize_env(bullet, RlItems)]
      )
    ).

% ---------------------------------------------------------------------------
% Special sections

fmt_customsect(DocSt) := Rc :-
    ( docst_currmod_is_main(DocSt) -> % Only in cover
      Rc = [htmlenv(hr, []),
            itemize_env(bullet,
                        ~fmt_toc_custom([changelog,
                                         bugs,
                                         references,
                                         copyright], DocSt))]
    ; Rc = []
    ).

fmt_toc_custom([], _, []).
fmt_toc_custom([N|Ns], DocSt, Rs) :-
    R = ~fmt_toc(single(N), DocSt),
    ( doctree_is_empty(R) ->
        Rs = Rs0
    ; Rs = [item_env(default, R)|Rs0] % TODO: to item_env?
    ),
    fmt_toc_custom(Ns, DocSt, Rs0).

% ---------------------------------------------------------------------------

:- doc(subsection, "Views of Table of Contents").
% This defines properties of different views of the table of contents (TOC)

% Title
:- discontiguous(toc_title/3).
% What kind of links are displayed in this toc
:- discontiguous(toc_link_filter/4).
% State variable containing the toc source
:- discontiguous(toc_source/2).

% Full TOC (title is in the section name)
toc_title(full, _, []). % (no title)
toc_source(full, full_toc_tree).
toc_link_filter(full, Props, Link, yes) :-
    \+ doclink_is_local(Link),
    \+ section_prop(is_special(_), Props).

% TOC for global links (to other pages)
% TODO: use different names depending on filetype
toc_title(global, application, string_esc("Parts of this manual")) :- !.
toc_title(global, part, string_esc("Subparts")) :- !.
toc_title(global, documentation, string_esc("Subparts")) :- !.
toc_title(global, module, string_esc("Submodules")) :- !.
toc_title(global, package, string_esc("Submodules")) :- !. % TODO: really?
toc_title(global, _, string_esc("Parts")) :- !. % include and package
toc_source(global, curr_toc_tree).
toc_link_filter(global, Props, Link, no) :-
    \+ doclink_is_local(Link),
    \+ section_prop(is_special(_), Props).

% TOC for local links (same page)
%toc_title(local, module, string_esc("Module Sections")) :- !.
%toc_title(local, package, string_esc("Package Sections")) :- !.
%toc_title(local, _, string_esc("Sections")) :- !.
toc_title(local, _, string_esc("ON THIS PAGE")) :- !.
toc_source(local, curr_toc_tree).
toc_link_filter(local, _Props, Link, yes) :-
    doclink_is_local(Link). 

% TOC for indices
toc_title(indices, _, string_esc("Indices")).
toc_source(indices, full_toc_tree).
toc_link_filter(indices, Props, _Link, yes) :-
    section_prop(is_special(index), Props).

% ---------------------------------------------------------------------------

:- doc(subsection, "Insertion of Command to show the Table of Contents").

% TODO: I am unsure about this part.

:- export(insert_show_toc/3).
:- pred insert_show_toc(R0, DocSt, R) # "Insert the command to show
   the table of contents in a given @pred{doctree/1}. The right place may be
   different depending on the chosen backend.".
insert_show_toc(R0, _DocSt, R) :-
    ( \+ custom_html_layout ->
        % TODO: @bug{menutexi} Not yet working, still needs external '.el'
        insert_show_toc_(R0, R)
    ; R = R0
    ).

% Insert 'show_toc' in the right place
insert_show_toc_(R0, R) :-
    R0 = section_env(PrevSecProps, SectLabel, TitleR, Body0),
    Toc = show_toc(subparts),
    doctree_insert_before_subfile_section(Body0, Toc, Body),
%       doctree_insert_end(Body0, Toc, Body),
    R = section_env(PrevSecProps, SectLabel, TitleR, Body).

% ---------------------------------------------------------------------------

:- doc(subsection, "Auxiliary for Mathematical Notation").
% TODO: Include in the parser?

%:- export(fmt_to_latex/2).
% Using '\' in lpdoc may be tedious when appearing in strings ("...").
% We allow '@' as control character for formulas. This predicate translates '@'
% to '\' (except '@@' and '\@').
fmt_to_latex([], []).
fmt_to_latex("\\@"||Xs, "\\@"||Ys) :- !, % avoid translation of @
    fmt_to_latex(Xs, Ys).
fmt_to_latex("@@"||Xs, "@"||Ys) :- !, % avoid translation of second @
    fmt_to_latex(Xs, Ys).
fmt_to_latex("@"||Xs, "\\"||Ys) :- !, % translate @ to \
    fmt_to_latex(Xs, Ys).
fmt_to_latex([X|Xs], [X|Ys]) :- !,
    fmt_to_latex(Xs, Ys).

% ===========================================================================

% TODO: This code is not complete (it has problems with atoms, does not
%       write lists correctly, etc.)

% :- pred sp_write/2 : stream * term 
% 
%    # "Same as @pred{write/2}, but puts space around operators and
%       between functor arguments. This makes them easier to read in
%       documents.".
% 
% sp_write(OS,T) :-
%         nonvar(T),
%         functor(T, F, A),
%         sp_write_(OS,A, F, T), !.
% sp_write(OS,T) :- write(OS,T).
% 
% sp_write_(OS,1, F, T) :-
%         current_postfixop(F, P, _), !,
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]),
%         put_code(OS,0' ),
%         display(OS,F).
% sp_write_(OS,1, F, T) :-
%         current_prefixop(F, _, P), !,
%         display(OS,F),
%         put_code(OS,0' ),
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]).
% sp_write_(OS,2, F, T) :-
%         current_infixop(F, P, _, Q), !,
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]),
%         put_code(OS,0' ),
%         display(OS,F),
%         put_code(OS,0' ),
%         arg(2, T, B),
%         write_term(OS, B, [priority(Q), numbervars(true)]).
% sp_write_(OS,A, F, T) :-
%         display(OS, F),
%         put_code(OS,0'(),
%         sp_write_args(1,A,OS,T),
%         put_code(OS,0')).
% 
% sp_write_args(A,A,OS,T) :- !,
%         arg(A,T,Ta),
%         write(OS,Ta).
% sp_write_args(E,A,OS,T) :-
%         arg(E,T,Te),
%         write(OS,Te),
%         display(OS,', '),
%         E1 is E+1,
%         sp_write_args(E1,A,OS,T).

% ===========================================================================

:- doc(bug, "@tt{biblio_pairs} should be a dictionary, not a plain list").
