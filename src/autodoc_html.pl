:- module(autodoc_html, [], [assertions, regtypes, fsyntax]).
% (Nothing is exported, because everything works using hooks)

:- doc(title, "HTML backend").
:- doc(author, "Jose F. Morales").

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_index)).
:- use_module(lpdoc(autodoc_refsdb)).
:- use_module(lpdoc(autodoc_images)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_messages), [autodoc_message/3]).
:- use_module(library(lists), [member/2, append/3, list_concat/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(dict)).
:- use_module(library(pathnames), [path_basename/2]).
:- use_module(lpdoc(comments), [stringcommand/1]).
:- use_module(library(format_to_string), [format_to_string/3]).
%
:- use_module(library(syntax_highlight),
    [can_highlight/1, highlight_string_to_html_string/3]).

% (Web-site extensions)
:- use_module(lpdoc(autodoc_html_template)).

% ======================================================================

:- multifile autodoc_escape_string_hook/5.

autodoc_escape_string_hook(html, _InputType, NS, _DocSt, VS) :- !,
    html_escape(NS, VS).

% ======================================================================

:- multifile autodoc_rw_command_hook/4.

:- pred autodoc_rw_command_hook(Backend, DocSt, Command, NewCommand)
    : backend_id * docstate * doctree * doctree.

autodoc_rw_command_hook(html, DocSt, Command, NewCommand) :- !,
    rw_command(Command, DocSt, NewCommand).

% ......................................................................

rw_command(ref_link(Link, Text), DocSt, R) :- !,
    fmt_link(default, no_label, Link, DocSt, string_esc(Text), R).
rw_command(missing_link(Text), DocSt, R) :- !,
    fmt_link('lpdoc-missing', no_label, no_link, DocSt, string_esc(Text), R).
% TODO: 'sp' replaced by just 'p', which yield better documents
rw_command(sp(_), _, R) :- !, R = raw("<p>").
%rw_command(sp(NS), _, R) :- !,
%       number_codes(N, NS),
%       N1 is N+1,
%       html_blank_lines(N1, NewCommand),
%       R = raw(NewCommand).
rw_command(p(""),                _, raw("<p>")) :- !.
rw_command(codeblock(Lang, Text), DocSt, R) :- !,
    fmt_codeblock(Lang, Text, DocSt, R).
rw_command(mathenv(S),           _, R) :- !,
    % environment using MathJax (in-line formula)
    R = htmlenv(script, [type="math/tex"], raw(S)).
rw_command(mathenv(display,S),   _, R) :- !,
    % environment using MathJax (long formula)
    R = htmlenv(script, [type="math/tex; mode=display"], raw(S)).
rw_command(defmathcmd_(Cmd,"0",Def), _, R) :- !,
    % new math command (MathJax)
    R = htmlenv(script, [type="math/tex"], [
          raw("\\newcommand{"), raw(Cmd), raw("}{"), raw(Def), raw("}")
        ]).
rw_command(defmathcmd_(Cmd,N,Def), _, R) :- !,
    % new math command (MathJax)
    R = htmlenv(script, [type="math/tex"], [
          raw("\\newcommand{"), raw(Cmd), raw("}["), raw(N), raw("]{"), raw(Def), raw("}")
        ]).
rw_command(noindent(""),           _, nop) :- !.
rw_command(newblock(""),           _, [raw("<br/>")]) :- !. % TODO: remove? just for bibrefs
rw_command(env_('itemize', X),     _, htmlenv(ul, X)) :- !.
rw_command(env_('enumerate', X),   _, htmlenv(ol, X)) :- !.
rw_command(env_('description', X), _, htmlenv(dl, X)) :- !.
rw_command(env_('cartouche', X),   _, cartouche(X)) :- !.
rw_command(env_('note', X), _, note(X)) :- !.
rw_command(env_('alert', X), _, alert(X)) :- !.
rw_command(env_('verbatim', X),     _, htmlenv(pre, [class="lpdoc-codeblock"], X)) :- !.
rw_command(item(S), _DocSt, NBody) :- !, % (items for lists and descriptions)
    % TODO: use item_env
    ( doctree_is_empty(S) ->
        NBody = raw("<li>")
    ; NBody = [raw("<dt>"), S, raw("<dd>")]
    ).
rw_command(item_env(Style, X), _DocSt, NBody) :- !, % (items for lists)
    ( Style = selmenu ->
        NBody = htmlenv(li, [role="presentation", class="active"], X)
    ; Style = unselmenu ->
        NBody = htmlenv(li, [role="presentation"], X)
    ; NBody = htmlenv(li, X)
    ).
rw_command(item_num(S), _,   NBody) :- !, % (items for enumerations)
    ( S = "" -> Props = [] ; Props = [value=S] ),
    NBody = htmlenv0(li, Props).
rw_command(footnote(Text), _DocSt, NBody) :- !,
    NBody = [raw("<p>"), htmlenv(b, raw("Note:")), raw(" "), Text, raw("<p>")].
rw_command('}',                   _, raw("}")) :- !.
rw_command('{',                   _, raw("{")) :- !.
rw_command('@',                   _, raw("@")) :- !.
rw_command('\\',                  _, raw("\\")) :- !.
rw_command(today(""),             _, R) :- !, fmt_date(R).
rw_command(hfill(""),             _, raw("")) :- !.
rw_command('`'([X]),              _, raw("&"||([X|"grave;"]))) :- !.
rw_command(''''([X]),             _, raw("&"||([X|"acute;"]))) :- !.
% NOTE: Escaped ^ due to fsyntax!
rw_command(^'^'([X]),             _, raw("&"||([X|"circ;"]))) :- !.
rw_command('..'([X]),             _, raw("&"||([X|"uml;"]))) :- !.
rw_command('"'([X]),              _, raw("&"||([X|"uml;"]))) :- !.
% NOTE: Escaped ~ due to fsyntax!
rw_command(^'~'([X]),             _, raw("&"||([X|"tilde;"]))) :- !.
rw_command('='([X]),              _, raw("&"||([X|"macr;"]))) :- !.
rw_command('.'([X]),              _, raw([X|"&#775;"])) :- !.
rw_command('u'([X]),              _, raw([X|"&#774;"])) :- !.
rw_command('v'([X]),              _, raw([X|"&#780;"])) :- !.
rw_command('H'([X]),              _, raw([X|"&#779;"])) :- !.
rw_command('t'([X, Y]),           _, raw([X|("&#865;"||[Y])])) :- !.
rw_command('c'([X]),              _, raw([X|"&#807;"])) :- !.
rw_command('d'([X]),              _, raw([X|"&#803;"])) :- !.
rw_command('b'([X]),              _, raw([X|"&#817;"])) :- !.
rw_command('oe'(""),              _, raw("&oelig;")) :- !.
rw_command('OE'(""),              _, raw("&OElig;")) :- !.
rw_command('ae'(""),              _, raw("&aelig;")) :- !.
rw_command('AE'(""),              _, raw("&AElig;")) :- !.
rw_command('aa'(""),              _, raw("&aring;")) :- !.
rw_command('AA'(""),              _, raw("&Aring;")) :- !.
rw_command('o'(""),               _, raw("&oslash;")) :- !.
rw_command('O'(""),               _, raw("&Oslash;")) :- !.
rw_command('l'(""),               _, raw("&lstrok;")) :- !.
rw_command('L'(""),               _, raw("&Lstrok;")) :- !.
rw_command('ss'(""),              _, raw("&szlig;")) :- !.
rw_command('?'(""),               _, raw("&iquest;")) :- !.
rw_command('!'(""),               _, raw("&iexcl;")) :- !.
rw_command('i'(""),               _, raw("&inodot;")) :- !.
rw_command('j'(""),               _, raw("&jmath;")) :- !.
rw_command(copyright(""),         _, raw("&#169;")) :- !.
rw_command(iso(""), _, R) :- !, R = htmlenv(span, [class="lpdoc-iso"], [raw("ISO")]).
rw_command(bullet(""), _,       raw("&#186;")) :- !.
rw_command(result(""), _,       raw("&rArr;")) :- !. % =>
rw_command(href(URL), _DocSt, NBody) :- !,
    NBody = htmlenv(a, [href=URL], [raw(URL)]).
rw_command(href(URL, Text), _DocSt, NBody) :- !,
    NBody = htmlenv(a, [href=URL], Text).
rw_command(email(Address), _DocSt, NBody) :- !,
    NBody = [raw("<a href=""mailto:"), Address, raw(""">&lt;"), Address, raw("&gt;</a>")].
rw_command(email(Text, Address), _DocSt, NBody) :- !,
    NBody = [raw("<a href=""mailto:"), Address, raw(""">"), Text, raw("</a>")].
rw_command(image_auto(IFile0, Opts), DocSt, NBody) :- !,
    ( locate_and_convert_image(IFile0, ['.png', '.jpg'], DocSt, IFile) -> 
        ( Opts = [] ->
            NBody = [raw("<img src="""), raw(IFile), raw(""">")]
        ; Opts = [Width, Height] ->
            NBody = [raw("<img src="""), raw(IFile), raw(""" width="), raw(Width),
                     raw(" height="), raw(Height), raw(">")]
        )
    ; autodoc_message(error, "-> Adding dummy text for missing image ~w in html", [IFile0]),
      NBody = [htmlenv(tt, [raw("[ Image: "),raw(IFile0),raw(" ]<br //>")])]
      % NBody = [raw("[ Image: "),raw(IFile0),raw(" ]<br //>")]
    ).
rw_command(bf(Body),  _DocSt, R) :- !, R = htmlenv(strong, Body).
rw_command(em(Body),  _DocSt, R) :- !, R = htmlenv(em, Body).
rw_command(tt(Body),  _DocSt, R) :- !, R = htmlenv(tt, Body).
rw_command(key(Body), _DocSt, R) :- !, R = htmlenv(span, [class="lpdoc-emacskey"], Body).
rw_command(var(Body), _DocSt, R) :- !, R = htmlenv(span, [class="lpdoc-var"], Body).
%
% TODO: Move to a doc_module?
rw_command(html_template(FileC), _DocSt, R) :- !,
    atom_codes(File, FileC),
    fmt_html_template(File, [], R).
% TODO: Move to a doc_module?
rw_command(html_template_internal(File, Params), _DocSt, R) :- !,
    fmt_html_template(File, Params, R).
% LPdoc command @tmplvar{Name} (for <v>Name</v> Ciao html templates)
% TODO: Move to a doc_module?
rw_command(tmplvar(Name), _DocSt, R) :- !,
    R = [raw_string("<v>"), raw_string(Name), raw_string("</v>")].
% .......... (icmd) ..........
% TODO: Share common definitions with autodoc_texinfo
rw_command(htmlenv(Cmd, Body), _, NewCommand) :- !, % <cmd>BODY</cmd>
    atom_codes(Cmd, CmdS),
    NewCommand = [raw("<"), raw(CmdS), raw(">"), Body, raw("</"), raw(CmdS), raw(">")].
rw_command(htmlenv(Cmd, Props, Body), DocSt, NewCommand) :- !, % <cmd PROPS>BODY</cmd>
    atom_codes(Cmd, CmdS),
    fmt_html_props(Props, DocSt, PropsR),
    NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw(">"), Body, raw("</"), raw(CmdS), raw(">")].
rw_command(htmlenv1(Cmd), _, NewCommand) :- !, % <cmd/>
    atom_codes(Cmd, CmdS),
    NewCommand = [raw("<"), raw(CmdS), raw("/>")].
rw_command(htmlenv1(Cmd, Props), DocSt, NewCommand) :- !, % <cmd PROPS/>
    atom_codes(Cmd, CmdS),
    fmt_html_props(Props, DocSt, PropsR),
    NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw("/>")].
rw_command(htmlenv0(Cmd, Props), DocSt, NewCommand) :- !, % <cmd PROPS> % TODO: Valid syntax anymore?
    atom_codes(Cmd, CmdS),
    fmt_html_props(Props, DocSt, PropsR),
    NewCommand = [raw("<"), raw(CmdS), raw(" "), PropsR, raw(">")].
%
rw_command(htmldecl(C), _, NewCommand) :- !,
    NewCommand = [raw("<!"), raw(C), raw(">")].
rw_command(htmlcomment(C), _, NewCommand) :- !,
    NewCommand = [raw("<!-- "), raw(C), raw(" -->")].
%
rw_command(section_env(SecProps, SectLabel, TitleR, Body), DocSt, R) :- !,
    fmt_section_env(SecProps, SectLabel, TitleR, Body, DocSt, R).
rw_command(backend_include_component(_), _DocSt, nop) :- !.
rw_command(hfill, _DocSt, R) :- !, % vertical space
    % TODO: finish
    R = raw(" ").
rw_command(linebreak, _DocSt, R) :- !,
    R = raw("<br/>").
rw_command(subsection_title(X), _DocSt, R) :- !,
    R = htmlenv(h2, X).
rw_command(twocolumns(X), _DocSt, R) :- !,
    R = htmlenv(div, [class="lpdoc-twocolumns"], X).
rw_command(itemize_env(none, Xs), _DocSt, R) :- !,
    R = htmlenv(ul, [class="lpdoc-itemize-none"], Xs).
rw_command(itemize_env(plain, Xs), _DocSt, R) :- !,
    R = htmlenv(ul, [class="lpdoc-itemize-plain"], Xs).
rw_command(itemize_env(minus, Xs), _DocSt, R) :- !,
    R = htmlenv(ul, [class="lpdoc-itemize-minus"], Xs).
rw_command(itemize_env(menu, Xs), _DocSt, R) :- get_layout(tmpl_layout(_, _, _)), !,
    R = htmlenv(ul, [class="nav nav-pills flex-column"], Xs). % TODO: only for bootstrap framework CSS
rw_command(itemize_env(horizontal_menu, Xs), _DocSt, R) :- !,
    R = htmlenv(ul, [class="lpdoc-horiz-menu"], Xs).
rw_command(itemize_env(sectpath, Xs), _DocSt, R) :- !, % TODO: special case
    R = htmlenv(ul, [class="lpdoc-itemize-sectpath"], Xs).
rw_command(itemize_env(_, Xs), _DocSt, R) :- !,
    R = htmlenv(ul, Xs).
rw_command(description_env(Xs), _DocSt, R) :- !,
    R = htmlenv(dl, Xs).
rw_command(cartouche(X), _DocSt, R) :- !,
    R = htmlenv(div, [class="lpdoc-cartouche"], X).
rw_command(optional_cartouche(X), _DocSt, R) :- !,
    R = cartouche(X).
rw_command(note(X), _DocSt, R) :- !,
    R = htmlenv(div, [class="lpdoc-note"], X).
rw_command(alert(X), _DocSt, R) :- !,
    R = htmlenv(div, [class="lpdoc-alert"], X).
rw_command(bibitem(Label,Ref), DocSt, R) :- !,
    Type = cite,
    IdxLabel = local_label(Ref),
    OutLink = ~get_use_outlink(Type, Ref, DocSt),
    R0 = [string_esc("["), string_esc(Label), string_esc("]")],
    R1 = ~fmt_link('lpdoc-idx-anchor', IdxLabel, OutLink, DocSt, R0),
    R = [item(htmlenv(strong, [id=Ref], R1))]. % TODO: use item_env?
rw_command(idx_anchor(_Mode, _Type, IdxLabel, _Key, OutLink, Text), DocSt, R) :- !,
    fmt_link('lpdoc-idx-anchor', IdxLabel, OutLink, DocSt, Text, R).
rw_command(cover_title(TitleR, SubtitleRs), _DocSt, R) :- !,
    R = htmlenv(div, [class="lpdoc-cover-title"], [
          htmlenv(h1, [class="lpdoc-cover-h1"], TitleR)
          |Rs]),
    sep_nl(SubtitleRs, Rs).
rw_command(cover_subtitle_extra(Rs), _DocSt, R) :- !,
    sep_nl(Rs, R).
rw_command(authors(AuthorRs), _DocSt, R) :- !,
    sep_nl(AuthorRs, R).
rw_command(backend_comment(_String), _DocSt, R) :- !,
    R = nop.
rw_command(quotation(X), _DocSt, R) :- !,
    R = htmlenv(div, X).
rw_command(left_and_right(Left, Right), _DocSt, R) :- !,
    R = [htmlenv(span, [class="lpdoc-on-right"], Right),
         htmlenv(span, Left)].
rw_command(navigation_env(Left, Right), _DocSt, R) :- !,
    R = [htmlenv(div, [class="lpdoc-nav"], [
           htmlenv(span, [class="lpdoc-on-right"], Right),
           htmlenv(span, Left)])].
rw_command(defpred(IdxLabel, Type, Text, PN, HeadR, Body), DocSt, R) :- !,
    PN = F/A, format_to_string("~w/~w", [F, A], S),
    %
    OutLink = ~get_use_outlink(Type, S, DocSt),
    R0 = idx_anchor(def, Type, IdxLabel, string_esc(S), OutLink, string_esc(S)),
    %
    fmt_usage_decl(HeadR, UsageDecl),
    R = [htmlenv(div, [
           htmlenv(div, [class="lpdoc-defname"], [
             htmlenv(span, [class="lpdoc-predtag"], [raw(Text)]),
             R0
           ]),
%              linebreak,
           htmlenv(div, [class="lpdoc-deftext"], [UsageDecl, Body])
         ])
        ].
rw_command(defassrt(Status, StatusStr, UsageStr, HeadR, DescR, UsageProps), _DocSt, R) :- !,
    ( StatusStr = "" -> HeaderR1 = HeaderR0
    ; ( Status = true -> StatusStyle = "lpdoc-true-header"
      ; Status = false -> StatusStyle = "lpdoc-false-header"
      ; Status = check -> StatusStyle = "lpdoc-check-header"
      ; Status = checked -> StatusStyle = "lpdoc-checked-header"
      ; Status = trust -> StatusStyle = "lpdoc-trust-header"
      ; throw(error(unknown_assrt_status(Status), rw_command/3))
      ),
      HeaderR1 =
        [htmlenv(span, [class=StatusStyle], [string_esc(StatusStr)])|HeaderR0]
    ),
    %KK HERE show status and usage; then disable status with doccomments
    ( UsageStr = "" -> HeaderR0 = []
    ; HeaderR0 = [htmlenv(span, [class="lpdoc-usage-header"], [string_esc(UsageStr)])]
    ),
    % add optional p("")
    ( HeaderR1 = [] -> HeaderR = HeaderR1
    ; HeaderR = [p("")|HeaderR1]
    ),
    fmt_usage_decl(HeadR, UsageDecl),
    R = [HeaderR,
         UsageDecl,
         htmlenv(p, DescR),
         UsageProps].
rw_command(assrtprops(DPR, CPR, APR, NGPR), _DocSt, R) :- !,
    R = itemize_env(minus, [
           DPR,
           CPR,
           APR,
           NGPR
        ]).
%
rw_command(simple_link(Style, Label, Link, Title), DocSt, R) :- !,
    fmt_link(Style, Label, Link, DocSt, Title, R).
rw_command(X, DocSt, _R) :- !,
    docst_currmod(DocSt, Name),
    throw(error(not_in_domain_rw_command(html, Name, X), rw_command/3)).

fmt_usage_decl(HeadR, UsageDecl) :-
    ( doctree_is_empty(HeadR) -> UsageDecl = []
    ; UsageDecl = [htmlenv(span, [class="lpdoc-usage-decl"], HeadR)]
    ).

:- pred fmt_link(Style, IdLabel, Link, DocSt, Text, R) ::
    atm * doclabel * doclink * docstate * doctree * doctree
   # "@var{R} is a hyperlink showing text @var{Text}, pointing to
     @var{Link} and identified with @var{IdLabel}.".

fmt_link(Style, IdLabel, Link, DocSt, Text, R) :-
    % The style for this element
    ( Style = default ->
        Props = Props0
    ; atom_codes(Style, StyleC),
      Props = [class=StyleC|Props0]
    ),
    % The identifier of this element (e.g. for incoming links)
    doclabel_to_html_id(IdLabel, Id),
    ( Id = "" ->
        Props0 = Props1
    ; Props0 = [id=Id|Props1]
    ),
    %
    ( Link = no_link ->
        % no link, use a span env
        R = htmlenv(span, Props, Text)
    ; % The outcoming link from this element (i.e. when clicking)
      HRef0 = ~doctree_to_href(Link, DocSt),
      atom_codes(HRef1, HRef0),
      HRef = ~atom_codes(~prefix_htmlurl(HRef1)),
      Props1 = [href=HRef],
      R = htmlenv(a, Props, Text)
    ).

sep_nl([],     []).
sep_nl([R|Rs], [R2|Rs2]) :-
    R2 = [R, raw("<br/>")],
    sep_nl(Rs, Rs2).

fmt_html_props([], _, []) :- !.
fmt_html_props([P0], DocSt, [P]) :- !,
    fmt_html_prop(P0, DocSt, P).
fmt_html_props([P0|Ps0], DocSt, [P, raw(" ")|Ps]) :- !,
    fmt_html_prop(P0, DocSt, P),
    fmt_html_props(Ps0, DocSt, Ps).
fmt_html_props(Ps, _, _) :-
    throw(error(bad_html_props(Ps), fmt_html_props/3)).

fmt_html_prop(Attr=Val, _DocSt, R) :- !,
    atom_codes(Attr, AttrS),
    % TODO: Missing escape " in Val
    R = [raw(AttrS), raw("=\""), raw(Val), raw("\"")].
fmt_html_prop(attr(Attr,Val), _DocSt, R) :- !, % TODO: like =/2 but process the value
    atom_codes(Attr, AttrS),
    % TODO: Missing escape " in Val
    R = [raw(AttrS), raw("=\""), Val, raw("\"")].
fmt_html_prop(P, _, _) :-
    throw(error(bad_html_prop(P), fmt_html_props/3)).

% TODO: refine this code
fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, ModR) :-
    section_prop(file_top_section, SecProps),
    !,
    fmt_top_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, ModR).
fmt_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, R) :-
    fmt_section(SecProps, SectLabel, TitleR, BodyR, DocSt, R).

sec_is_cover(SecProps) :-
    section_prop(coversec(_,_,_,_,_,_,_), SecProps),
    !.

% ---------------------------------------------------------------------------

fmt_codeblock(Lang, Text, DocSt, R) :- Lang = "ciao_runnable", !, % TODO: document
    % (special case for blocks runnable by ciao_playground)
    R = htmlenv(pre, [class="lpdoc-codeblock-runnable"], TextR),
    escape_string(normal, Text, DocSt, NText),
    TextR = raw_string(NText).
fmt_codeblock(Lang, Text, DocSt, R) :-
    R = htmlenv(pre, [class="lpdoc-codeblock"], TextR),
    ( try_highlight(Lang, Text, TextR0) ->
        TextR = TextR0
    ; % TODO: sometimes type should be 'verb'? (forbid 'verbatim'?)
      escape_string(normal, Text, DocSt, NText),
      TextR = raw_string(NText)
    ).

try_highlight(Lang, Text, TextR) :-
    ( atom_codes(LangAtm, Lang),
      \+ LangAtm = 'text',
      can_highlight(LangAtm),
      \+ setting_value(syntax_highlight, no) -> % (default is 'yes')
        ( highlight_string_to_html_string(LangAtm, Text, HtmlStr) ->
            TextR = raw(HtmlStr)
        ; autodoc_message(error,"Could not highlight code block for ~w syntax", [LangAtm]),
          fail
        )
    ; fail
    ).

% ---------------------------------------------------------------------------

% Get Layout
get_layout(Layout) :-
    ( setting_value(html_layout, Layout0) -> Layout = Layout0
    ; Layout = nav_sidebar_main
    ),
    valid_layout(Layout).

valid_layout(embedded).
valid_layout(nav_sidebar_main).
valid_layout(website_layout(_)).
valid_layout(tmpl_layout(_, _, _)).

% ---------------------------------------------------------------------------

% :- trait doclayout.
:- discontiguous layout_toc/2.
:- discontiguous layout_has_colophon/2.
:- discontiguous layout_sidebar_pos/2. % (fail if no sidebar)
:- discontiguous layout_icon/2.
:- discontiguous layout_css_url/2.

layout_toc(embedded) := R :- !, R = []. % no toc
layout_toc(nav_sidebar_main) := R :- !, R = show_toc(sidebar).
layout_toc(website_layout(Opts)) := R :- !, ( member(vertical_navmenu, Opts) -> R = show_toc(navmenu(vertical)) ; R = [] ).
layout_toc(tmpl_layout(_, _, _)) := R :- !, R = show_toc(navmenu(vertical)).

layout_has_colophon(embedded) :- fail.
layout_has_colophon(nav_sidebar_main).
layout_has_colophon(website_layout(_)).
layout_has_colophon(tmpl_layout(_,_,_)) :- fail.

layout_sidebar_pos(embedded, _) :- fail.
% TODO: make it customizable
layout_sidebar_pos(nav_sidebar_main, fixleft).
%layout_sidebar_pos(nav_sidebar_main, right).
layout_sidebar_pos(website_layout(Opts), right) :- member(vertical_navmenu, Opts), !.
layout_sidebar_pos(tmpl_layout(_,_,_), _) :- fail.

% (nondet)
layout_icon(website_layout(Opts), X) :-
    ( member(icon(Icon), Opts), atom(Icon) -> % TODO: document (max 1)
        X = Icon
    ; fail
    ).

% (nondet)
layout_css_url(website_layout(Opts), URL) :-
    member(css(URL), Opts). % TODO: document
layout_css_url(tmpl_layout(_, _, CssList), URL) :-
    member(URL, CssList).

layout_fixed_sidebar(Layout) :-
    layout_sidebar_pos(Layout, SidebarPos),
    fixed_sidebar_pos(SidebarPos),
    !.

fixed_sidebar_pos(fixleft).
fixed_sidebar_pos(fixright).

% ---------------------------------------------------------------------------

% Title for page (header metadata, for browser window, search results, etc.)
fmt_page_title(SecProps, TitleR, DocSt, R) :-
    ( docst_gdata_query(DocSt, main_title(MainTitleR)) ->
        true
    ; throw(error(no_main_title, fmt_page_title/4))
    ),
    ( sec_is_cover(SecProps) ->
        R = MainTitleR
    ; R = [TitleR, raw(" &mdash; "), MainTitleR]
    ).

fmt_top_section_env(SecProps, SectLabel, TitleR, BodyR, DocSt, ModR) :-
    get_layout(Layout),
    % Title
    fmt_page_title(SecProps, TitleR, DocSt, PageTitleR),
    % Sidebar
    fmt_sidebar(Layout, SecProps, DocSt, SidebarR),
    % Main content holder
    fmt_main(Layout, SecProps, SectLabel, TitleR, BodyR, DocSt, MainR),
    %
    fmt_layout(Layout, SidebarR, TitleR, MainR, DocSt, R),
    fmt_headers(Layout, PageTitleR, R, ModR).

% Format the sidebar
fmt_sidebar(Layout, SecProps, DocSt, R) :-
    % Optional image on sidebar
    % TODO: remove pragmas, define new comment types instead
    ( section_prop(pragmas(Pragmas), SecProps) -> true ; Pragmas = [] ),
    ( member(section_image(SectImg), Pragmas) ->
        ImgSrc = ~atom_codes(~img_url(SectImg)),
        PreSect = htmlenv(div, [style="text-align: center;"], 
                          htmlenv1(img, [src=ImgSrc, class="lpdoc-section-image"]))
    % ; sec_is_cover(SecProps) -> PreSect = ~fmt_sidebar_cover_logo(DocSt) %(nocoverlogo)
    ; PreSect = ~fmt_sidebar_logo(DocSt)
    ),
    %
    TocR = ~layout_toc(Layout),
    %
    doctree_simplify([PreSect, TocR], R).

% Format the sidebar logo (if any)
fmt_sidebar_logo(DocSt, R) :-
    ( docst_gdata_query(DocSt, main_logo(Logo)) ->
        R = ~fmt_sidebar0([image(~atom_codes(Logo), "auto", "100%")])
    ; R = []
    ).

% % Format the cover sidebar logo (blank space if we have logo)
% fmt_sidebar_cover_logo(DocSt, R) :-
%       ( docst_gdata_query(DocSt, main_logo(_)) ->
%           R = ~fmt_sidebar0([raw("&nbsp;")])
%       ; R = []
%       ).

fmt_sidebar0(R) :=
    % (Limit height to 40px)
    htmlenv(div, [style="height: 40px; margin-left: auto; margin-right: auto"], R).

% Format main content holder
fmt_main(Layout, SecProps, SectLabel, TitleR, BodyR, DocSt, MainR) :-
    ( Layout = website_layout(_) ->
        MainR = [htmlenv(h1, TitleR), raw_nl, BodyR] % TODO: Hardwired, fix
    ; Layout = embedded ->
        MainR = [/*htmlenv(h1, TitleR), raw_nl, */BodyR] % No title
    ; Layout = tmpl_layout(_, _, _) ->
        MainR = BodyR
    ; ( sec_is_cover(SecProps) ->
          fmt_cover(SecProps, TitleR, BodyR, DocSt, MainR)
      ; fmt_section(SecProps, SectLabel, TitleR, BodyR, DocSt, MainR)
      )
    ).

% Format a module as a cover
fmt_cover(SecProps, TitleR, BodyR, _DocSt, R) :-
    section_prop(coversec(SubtitleRs,
                          SubtitleExtraRs,
                          AuthorRs,
                          AddressRs,
                          GVersShortR,
                          _GVersR,
                          _CopyrightR),
                 SecProps),
    % Add version (GVers) to subtitle
    ( doctree_is_empty(GVersShortR) ->
        GVersShortRs = []
    ; GVersShortRs = [GVersShortR]
    ),
    % Address box (optional)
    ( AddressRs = [] ->
        AddressRs2 = []
    ; sep_nl(AddressRs, AddressRs1),
      AddressRs2 = htmlenv(div, [class="lpdoc-cover-address"], AddressRs1)
    ),
    % CoverLogoR = ~fmt_cover_logo(DocSt),
    % Document skeleton
    R = [
      htmlenv(div, [
        linebreak, % add some margin here
        % CoverLogoR,
        cover_title(TitleR, SubtitleRs),
        AddressRs2,
        htmlenv(div, [class="lpdoc-cover-authors"], [
          authors(AuthorRs)
        ]),
        htmlenv(div, [class="lpdoc-cover-subtitle-extra"], [
          cover_subtitle_extra(SubtitleExtraRs),
          cover_subtitle_extra(GVersShortRs)
        ]),
        htmlenv(div, [class="lpdoc-clearer"], [])
      ]),
      raw_nl,
      linebreak, % add some margin here
      linebreak, % add some margin here
      BodyR
    ].

% % Format the cover logo (if any)
% fmt_cover_logo(DocSt, R) :-
%       ( docst_gdata_query(DocSt, main_logo(Logo)) ->
%           R = htmlenv(div, [class="lpdoc-cover-logo"], [
%                 image(~atom_codes(Logo))
%                 ])
%       ; R = []
%       ).

% Navigation, sidebar, and main contents
fmt_layout(tmpl_layout(_, LayoutTmpl, _), SidebarR, TitleR, MainR, _DocSt, R) :- !,
    R = [html_template_internal(LayoutTmpl, [
           sidebar = SidebarR,
           title = TitleR,
           content = MainR])].
fmt_layout(embedded, _SidebarR, _TitleR, MainR, _DocSt, R) :- !,
    R = MainR.
fmt_layout(Layout, SidebarR, _TitleR, MainR, DocSt, R) :-
    fmt_topbar(Layout, DocSt, TopBarR),
    ( layout_sidebar_pos(Layout, SidebarPos),
      ( SidebarPos = left -> PageClass = "lpdoc-page leftbar"
      ; SidebarPos = right -> PageClass = "lpdoc-page rightbar"
      ; SidebarPos = fixleft -> PageClass = "lpdoc-page fixleftbar"
      ; SidebarPos = fixright -> PageClass = "lpdoc-page fixrightbar"
      ; fail
      ) ->
        SidebarToggleR = ~sidebar_toggle,
        SidebarEnvR = htmlenv(div, [id="sidebar", class="lpdoc-sidebar"], SidebarR)
    ; PageClass = "lpdoc-page",
      ( Layout = website_layout(_) -> % TODO: ad-hoc
          SidebarToggleR = ~sidebar_toggle
      ; SidebarToggleR = []
      ),
      SidebarEnvR = []
    ),
    footers(DocSt, Layout, FooterInnerR, FooterOuterR),
    doctree_simplify([%
         TopBarR, % top bar
         % NavTopR, % navigation at top
         htmlenv(div, [class=PageClass], [
           SidebarToggleR,
           SidebarEnvR,
           htmlenv(div, [class="lpdoc-main"], [
             % NavTopR, % navigation before main
             MainR,
             FooterInnerR % footer (for fixed sidebar)
           ]),
           htmlenv(div, [class="lpdoc-clearer"], [])
         ]),
         % NavBottomR, % navigation at bottom
         FooterOuterR % footer (for nonfixed sidebar)
        ], R).

% Footers (depending on layout)
footers(DocSt, Layout, FooterInnerR, FooterOuterR) :-
    ( layout_has_colophon(Layout) ->
        colophon(DocSt, FooterR),
        ( layout_fixed_sidebar(Layout) ->
            FooterInnerR = FooterR,
            FooterOuterR = []
        ; FooterInnerR = [],
          FooterOuterR = FooterR
        )
    ; FooterInnerR = [],
      FooterOuterR = []
    ).

% Top bar (optional, for logo, search box, horizontal navmenu, etc.)
fmt_topbar(website_layout(Opts), _DocSt, R) :- !,
    % TODO: generalize, any logo
    % TODO: auto 2x images
    LogoSrc = ~atom_codes(~img_url('ciao-logo.png')),
    LogoSrc2x = ~append(~atom_codes(~img_url('ciao-logo@2x.png')), " 2x"),
    IndexHRef = ~atom_codes(~prefix_htmlurl('index.html')),
    %
    ( member(vertical_navmenu, Opts) ->
        HorizNavMenuR = []
    ; HorizNavMenuR = [show_toc(navmenu(horizontal))]
    ),
    %
    R = htmlenv(div, [class="lpdoc-title"], [
      htmlenv(a, [href=IndexHRef], [
        htmlenv1(img, [src=LogoSrc,
                       srcset=LogoSrc2x,
                       height="64px",
                       'ALT'="Ciao",
                       class="lpdoc-logo"])
        ]),
      HorizNavMenuR
    ]).
fmt_topbar(_, _DocSt, []).

% colophon: "a the brief description of the publication or production
% notes relevant to the edition"
% (this is standard also for Web pages)
colophon(DocSt, R) :-
    ( docst_opt(no_lpdocack, DocSt) ->
        Ack = ""
    ; Ack = "Generated with LPdoc using Ciao"
    ),
    R = htmlenv(div, [class="lpdoc-footer"], [string_esc(Ack)]).

% TODO: Missing meta for website?
%   <meta charset="utf-8">
%   <meta http-equiv="X-UA-Compatible" content="IE=edge">
%   <meta name="keywords" content="prolog, compiler, constraint programming, declarative language, logic programming, programming-language, ciao">
%   <meta name="description" content="A general-purpose programming
%     language which supports logic, constraint, functional, higher-order,
%     and imperative programming styles. Additinally it offers a complete
%     Prolog system, supporting ISO-Prolog.">

fmt_headers(Layout, _PageTitleR, BodyR, R) :- Layout = embedded, % No headers
    !,
    R = BodyR.
fmt_headers(Layout, PageTitleR, BodyR, R) :- !,
    fmt_icon(~get_icon_list(Layout), IconR),
    fmt_css(~get_css_list(Layout), CssR),
    fmt_script(~get_script_list, ScriptR),
    MetaR = [IconR, CssR, ScriptR, htmlenv(title, PageTitleR)],
    fmt_headers_(Layout, MetaR, BodyR, R).

fmt_headers_(Layout, MetaR, BodyR, R) :-
    Layout = tmpl_layout(DocTmpl, _, _),
    !,
    R = [html_template_internal(DocTmpl, [head = MetaR, body = BodyR])].
fmt_headers_(_, MetaR, BodyR, R) :-
    MetaR2 = [
      htmlenv1(meta, ['http-equiv'="Content-Type", content="text/html; charset=utf-8"]),
      htmlenv1(meta, ['name'="viewport", content="width=device-width, initial-scale=1"]),
      htmlenv1(meta, ['name'="theme-color", content="#273f79"])|MetaR
    ],
    R = [
      raw("<!DOCTYPE HTML>"),
      htmlenv(html, [
        htmlenv(head, MetaR2),
        htmlenv(body, BodyR)
      ])
    ].

fmt_section(SecProps, SectLabel, TitleR, Body, _DocSt, R) :-
    doclabel_to_html_id(SectLabel, Id),
    fmt_structuring(SecProps, TitleR, StrR),
    R = htmlenv(div, [id=Id], [StrR, Body]).

fmt_structuring(SecProps, _TitleR, R) :-
    % No title in summary at cover
    section_prop(summary_section, SecProps), !,
    R = [].
fmt_structuring(SecProps, TitleR, R) :-
    ( section_prop(level(Level), SecProps) ->
        ( Level = 1 -> Cmd = h1
        ; Level = 2 -> Cmd = h2
        ; Level = 3 -> Cmd = h3
        ; Level = 4 -> Cmd = h4
        )
    ; throw(error(missing_level_prop(SecProps), fmt_structuring/3))
    ),
    R = htmlenv(Cmd, TitleR).

% From a doclabel, obtain a HTML element identifier
% in HTML, only local_label is significant; global_label is ignored
doclabel_to_html_id(local_label(Label), Id) :- !,
    Id = Label.
doclabel_to_html_id(localnum_label(Label), Id) :- !,
    Id = Label.
doclabel_to_html_id(_, Id) :- !, Id = "".

% From a doclink, obtain a HTML href 
doctree_to_href(Link, DocSt) := HRef :-
    Link = link_to(Base, SectLabel), !,
    docst_currmod(DocSt, Name),
    ( Base = url(URL) ->
        atom_codes(URL, HRef)
    ; Base = Name ->
        HRef = HRef0
    ; Backend = html,
      absfile_for_subtarget(Base, Backend, cr, F0),
      absfile_to_relfile(F0, Backend, F),
      atom_codes(F, FC),
      append(FC, HRef0, HRef)
    ),
    doclabel_to_html_id(SectLabel, SectId),
    ( SectId = "" ->
        HRef0 = ""
    ; HRef0 = "#"||SectId
    ).
doctree_to_href(no_link, _DocSt) := "#".

% ---------------------------------------------------------------------------

html_blank_lines(0, "") :- !.
html_blank_lines(N, "<br>"||R) :-
    N1 is N-1,
    html_blank_lines(N1, R).

%html_escape("``"||S0, "&ldquo;"||S) :- !, html_escape(S0, S).
%html_escape("''"||S0, "&rdquo;"||S) :- !, html_escape(S0, S).
html_escape([0'"|S0], "&quot;"||S) :- !, html_escape(S0, S).
html_escape([0''|S0], "&apos;"||S) :- !, html_escape(S0, S).
html_escape("&#"||S0, "&#"||S) :- !, html_escape(S0, S).
html_escape([0'&|S0], "&amp;"||S) :- !, html_escape(S0, S).
html_escape([0'<|S0], "&lt;"||S) :- !, html_escape(S0, S).
html_escape([0'>|S0], "&gt;"||S) :- !, html_escape(S0, S).
html_escape([X|S0], [X|S]) :- !,
    html_escape(S0, S).
html_escape([], []).

% ---------------------------------------------------------------------------

% Toggle button for sidebar
% TODO: use fmt_link?
sidebar_toggle(R) :-
    R = htmlenv(a, [href="#", id="sidebar-toggle-button", class="lpdoc-navbutton"], [
        htmlenv(span, [id="sidebar-button-arrow"], [raw("&#9776;")])
    ]).

% ===========================================================================
% Obtain the current date (for '@today' command)

:- use_module(library(system), [now/1, datime/9]).

% TODO: Share with other backends?
fmt_date(R) :-
    now(Time),
    datime(Time, Year, Month, Day, _Hour, _Min, _Sec, _, _),
    format_to_string("~w/~w/~w", [Year,Month,Day], S),
    R = string_esc(S).

% ===========================================================================
% Format some meta entries: icon, css, scripts

fmt_icon(IconList, R) :-
    ( IconList = [IconImg] ->
        IconHRef = ~atom_codes(~img_url(IconImg)),
        R = htmlenv1(link, [rel="shortcut icon", href=IconHRef])
    ; R = nop
    ).

fmt_css([], []).
fmt_css([X|Xs], [R|Rs]) :-
    HRef = ~atom_codes(~prefix_htmlurl(X)),
    R = htmlenv1(link, [rel="stylesheet", href=HRef, type="text/css"]),
    fmt_css(Xs, Rs).

fmt_script([], []).
fmt_script([S|Ss], [R|Rs]) :- fmt_script_(S, R), fmt_script(Ss, Rs).

fmt_script_(script(Type, url(URL)), R) :- !,
    Type2 = ~atom_codes(Type),
    URL2 = ~atom_codes(~prefix_htmlurl(URL)),
    R = htmlenv(script, [type=Type2, src=URL2], []).
fmt_script_(script(Type, inline(Src)), R) :- !,
    Type2 = ~atom_codes(Type),
    R = htmlenv(script, [type=Type2], [raw(Src)]).

% ===========================================================================
% Icons, CSS, JS
%
% TODO: asset_file/2 defines files that needs to be copied; the
%   following predicates does not necessarily mean that the files are
%   copied. Cleanup this code.

:- use_module(lpdoc(autodoc_html_assets), [asset_file/2]).

get_icon_list(Layout) := ~findall(Base, layout_icon(Layout, Base)).

get_css_list(Layout) := ~findall(Base, get_css_url(Layout, Base)).

get_script_list(ScriptList) :-
    findall(X, get_script(X), ScriptList).

% (nondet)
get_css_url(_Layout, URL) :-
    asset_file(css, F), path_basename(F, URL).
get_css_url(Layout, URL) :-
    layout_css_url(Layout, URL).

% (nondet)
get_script(X) :-
    % Enable runnable code blocks (ciao_playground.js) explitly (this
    % option is turned on by default if a lpdoc-codeblock-runnable is
    % detected dynamically)
    setting_value(allow_runnable, yes), % (default is 'no')
    X = script('text/javascript', inline("var lpdocPG='runnable';")).
get_script(script('text/javascript', url(URL))) :-
    asset_file(js, F), path_basename(F, URL).
get_script(X) :-
    % Mathematical notation
    % Note: currently, only MathJax is supported
    script_mathjax(X).

:- use_module(lpdoc(autodoc_html_assets), [using_mathjax/1]).

% Include mathjax, if available (for TeX output in HTML)
% (enum on nondet)
script_mathjax(X) :-
    using_mathjax(MathJaxJS),
    !,
    ( X = script('text/x-mathjax-config', inline(
            "MathJax.Hub.Config({"||
            " jax: [\"input/TeX\",\"output/HTML-CSS\"],"||
            " TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\"]}"||
            "});"))
    ; X = script('text/javascript', url(MathJaxJS))
    ).

% ===========================================================================

:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [create_link/2]).

:- multifile autodoc_is_operational_hook/2.
autodoc_is_operational_hook(html, _).

:- multifile autodoc_finish_hook/1.
autodoc_finish_hook(html) :- finish_html.

finish_html :-
    write_htmlmeta,
    index_symlink.

% Note: I added 'htmlmeta' so that there exist a node in
% the dependency graph (a directed graph) that is
% connected to all the files in the document. The 'cr'
% files are directly generated in '.html' format. That
% could be generalized to more backends. (JFMC)
write_htmlmeta :-
    Mod = ~get_mainmod,
    get_mainmod(Mod),
    format_get_file(htmlmeta, Mod, Out),
    %
    atom_codes(Mod, ModS),
    string_to_file(ModS, Out).

% Create a symlink from mainmod to index.html, if index does not
% exist.
index_symlink :-
    Mod = ~get_mainmod,
    get_mainmod(Mod),
    From = ~atom_concat(Mod, '.html'),
    To = ~absfile_for_aux('index.html', html),
    ( file_exists(To) -> true % TODO: Check if there is any module called 'index' instead?
    ; create_link(From, To)
    ).

:- use_module(library(stream_utils), [string_to_file/2]).

:- multifile autodoc_gen_alternative_hook/2.
% note: no alternative formats here
autodoc_gen_alternative_hook(html, _) :- fail.

% ===========================================================================

:- doc(bug, "hfill is not treated correctly.").

:- doc(bug, "Needed a better way to define new LaTex commands than
   this: @@begin@{displaymath@}
   @@newcommand@{@@neck@}@{@@textbf@{:-@}@} @@end@{displaymath@}
   (JFMC)").
