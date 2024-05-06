:- module(_, [], [doccfg]).

%! \title Config for LPdoc reference manual
%  \author Manuel Hermenegildo
%  \author Edison Mera
%  \author Jose F. Morales

:- include(core_docsrc(docpaths)).

filepath := '../readmes'.
filepath := '../../src'.
filepath := '../../examples'.
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
      'comments'-[
          'doccomments_doc' % Maturity?
      ],
      'Markdown',
      % 'assertions/assertions_doc'-[...] % Note: assertions moved to alldocs
      'lpdoc_examples',
%         'example_module',
      %
      % TODO: If we want to include the example output, it should be
      %         in a different manual so that we do not mess the
      %         current indices (e.g., q/1 is not a predicate from
      %         LPdoc). 
      %
      'Runnables'-[ 
          'factorial_peano_iso_source',
          'factorial_peano_iso'
      ]
%         'rtchecks_doc',
%         'unittest'-
%             ['unittest/unittest_props',
%               'unittestdecls_doc',
%               % 'unittest/unittest_utils',
%               'unittest/unittest_statistics',
%               'unittest/unittest_examples'
%            ],
    ].

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := yes.
syntax_highlight := yes.
