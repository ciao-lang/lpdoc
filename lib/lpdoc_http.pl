 :- package(lpdoc_http). % TODO: :- trait lpdoc_http.
:- use_package(library(http/http_service)). % TODO: :- extends http_service.

% Trait for HTTP-based interface for LPdoc templates
% NOTE: currently it just instantiates holes in a .html template
%   generated from a .pl/.lpdoc manual entry

% Requires:
%   :- pred service_name(Name) % (by http_service)
%   :- pred process_tmpl(Request, Info, TmplName, Subst).

:- use_module(library(http/http_forms), [http_parse_form/2]).
:- use_module(library(http/http_server), [http_protect/4]).
:- use_module(library(pillow/error_templates), [html_error_response/2]).

:- use_module(lpdoclib(lpdoc_http_rt), [page_response/3]).

treat_request(Request, Response) :-
	http_protect(treat_request_, error_response, Request, Response).

error_response(E, Response) :- html_error_response(E, Str), Response = html_string(Str).

treat_request_(Request, Response) :-
	http_parse_form(Request, Info),
	process_tmpl(Request, Info, TmplName, Subst),
	page_response(TmplName, Subst, Response).
