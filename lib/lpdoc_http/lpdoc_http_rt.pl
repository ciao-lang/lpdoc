:- module(_, [page_response/3], [assertions]).

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(pillow/html), [html_template/3, html2terms/2]).
:- use_module(ciaobld(config_common), [site_root_dir/1]). % TODO: move generated templates somewhere else? make sure files are there?

% Produce the HTTP response from the template and substitution
page_response(TmplName, Subst, Response) :-
    page_template(TmplName, Subst, HTML),
    html2terms(Str, HTML),
    Response = html_string(Str).

% Instantiate a LPdoc template
% TODO: ad-hoc solution (requires pregenerated .html)
page_template(TmplName, Subst, HTML) :-
    site_root_dir(SiteDir),
    path_concat(SiteDir, TmplName, TmplFile),
    file_to_string(TmplFile, TmplString),
    html_template(TmplString, HTML, Subst).

