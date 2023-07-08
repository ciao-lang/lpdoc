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
      'assertions/assertions_doc'-[
          'assertions/assertions_props',
          'regtypes/regtypes_doc',
          'basic_props',
          % 'assertions/native_props',
          % props native to analyzers, in chapters
          'assertions/native_props'-[
              'assertions/native_props_shfrg_doc',
              'assertions/native_props_nfdet_doc',
              'assertions/native_props_cardinality_doc',
              'assertions/native_props_exceptions_doc',
              'assertions/native_props_sideff_doc',
              'assertions/native_props_polyhedral_doc',
              'assertions/native_props_cost_doc'
          ],
          %
          'metaprops/meta_props'
      ],
      'lpdoc_examples',
      'Runnables'-[ 
          'factorial_peano_iso_source',
          'factorial_peano_iso'
      ]
      %
      % TODO: If we want to include it, it should be in a
      %         different manual so that we do not mess the
      %         current indices (e.g., q/1 is not a predicate from
      %         LPdoc)
%         'example_module',
      %
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
