% (included file)
:- doc(section, "CGI-based interface for LPdoc templates").

:- use_package(hiord).

% NOTE: currently it just intantiates holes in a .html template
%   generated from a .pl/.lpdoc manual entry

% (exported)
:- meta_predicate cgi_process_lpdoc(pred(3)).
cgi_process_lpdoc(Process) :-
        html_protect(cgi_process_lpdoc_(Process)).

:- meta_predicate cgi_process_lpdoc_(pred(3)).
cgi_process_lpdoc_(Process) :-
        get_form_input(Info),
	Process(Info, TmplName, Subst),
	page_template(TmplName, Subst, HTML),
 	output_html([cgi_reply, HTML]).

% ---------------------------------------------------------------------------
% LPdoc templates

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(pillow/html), [html_template/3]).
:- use_module(ciaobld(config_common), [site_root_dir/1]). % TODO: move generated templates somewhere else? make sure files are there?

% Instantiate a LPdoc template
% TODO: ad-hoc solution (requires pregenerated .html)
page_template(TmplName, Subst, HTML) :-
	site_root_dir(SiteDir),
	path_concat(SiteDir, TmplName, TmplFile),
	file_to_string(TmplFile, TmplString),
	html_template(TmplString, HTML, Subst).

