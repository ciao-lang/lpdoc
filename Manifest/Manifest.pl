:- bundle(lpdoc).
version('3.7.0').
depends([core]).
alias_paths([
    lpdoc = 'src',
    library = 'lib',
    lpdoc_etc = 'etc'
]).
cmd('lpdoc', [main='cmds/lpdoccl']).
lib('src').
lib('lib').
manual('lpdoc', [main='doc/reference/SETTINGS.pl']).
manual('lpdoc_devel', [main='doc/internals/SETTINGS.pl']).

