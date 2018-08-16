:- module(_, [], [doccfg]).

%! \title Config for LPdoc internals manual
%  \author Manuel Hermenegildo
%  \author Jose F. Morales

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := '../readmes'.
filepath := '../../src'.
filepath := '../../examples'.
filepath := ~ciaofilepath_common.

output_name := 'lpdoc_devel'.

doc_structure := 
	'lpdoc_devel_man'-[
	  'docmaker',
	  'autodoc',
	  'autodoc_state',
	  'autodoc_doctree',
	  'autodoc_structure',
	  'autodoc_settings',
	  % Backends
	  'Backends'-[
	    'autodoc_texinfo',
	    'autodoc_html'-[
	      'autodoc_html_assets',
	      'autodoc_html_template'
            ],
	    'autodoc_man'
          ],
	  % Miscellanea and other support code
	  'autodoc_filesystem',
	  'autodoc_index',
	  'autodoc_refsdb',
	  'autodoc_errors',
	  'autodoc_bibrefs',
	  'autodoc_aux',
	  'autodoc_images'
        ].

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
