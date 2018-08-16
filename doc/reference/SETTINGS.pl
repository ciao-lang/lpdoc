:- module(_, [], [doccfg]).

%! \title Config for LPdoc reference manual
%  \author Manuel Hermenegildo
%  \author Edison Mera
%  \author Jose F. Morales

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := '../readmes'.
filepath := '../../src'.
filepath := '../../examples'.
filepath := at_bundle(core, 'doc/common'). % (for InPrelude.lpdoc)
filepath := at_bundle(core, 'library/doccomments'). 
filepath := ~ciaofilepath_common.

output_name := 'lpdoc'.

doc_structure := 
	'lpdoc_ref_man'-[
	  'Generating',
	  'Writing'-[
		'doccfg/doccfg_doc', % (document the trait)
 	      'doccfg/doccfg_props' % (document option values)
          ],
	  'MarkUpDown'-[
		'comments',
		'doccomments_doc' % Maturity?
          ],
	  'AssrtDoc'-[
	      'assertions/assertions_doc',
	      'assertions/assertions_props',
	      'regtypes/regtypes_doc',
	      'basic_props',
	      'assertions/native_props',
	      'metaprops/meta_props'
          ],
          'lpdoc_examples'
	  %
	  % TODO: If we want to include it, it should be in a
          %	    different manual so that we do not mess the
          %	    current indices (e.g., q/1 is not a predicate from
          %	    LPdoc)
%	  'example_module',
	  %
%	  'rtchecks_doc',
%	  'unittest'-
%             ['unittest/unittest_props',
%		'unittestdecls_doc',
%		% 'unittest/unittest_utils',
%		'unittest/unittest_statistics',
%		'unittest/unittest_examples'
%	     ],
        ].

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
