# Changelog

## [3.9.0] - 2024-10-13

### Added

 - Slideshow view mode for HTML backend, including a slideshow
   button in the navigation bar for enabling "view as slides" mode.
   Slideshow allows keyboard navigation and a style for print media.
 - (lpdoc.css) Style for print media for slide mode.
 - Support for versioning URLs in lpdoc.js to invalidate
   browser caches. URLs with version suffixes (e.g., .../foo.html?v=1)
   allow invalidating browser caches when the version param is
   updated. That param is ignored when fetching data from the HTTP
   server.
 - Updated manual to mention new IDEs (Playground, VSCode).
 - .txt and .pdf as admissible formats for image handling.

### Improved

 - Documented some ALD tricks (not yet in the manual).
 - Shorter but more frequent progress messages (useful for
   large documents).
 - Predicates starting with `$` (normally internal predicates)
   are now not documented by default.

### Fixed 

 - Error messages related to no exports when there are doincludes.
 - Fix location of (some) figures in texinfo backend.
 - Updated `texinfo.tex` to avoid issues with dotless i, j.
 - Try `magick` then `convert` as ImageMagick command

## [3.8.0] - 2024-4-1

### Added

 - Added .txt and .pdf as admissible formats.
 - Cleanup of some unneeded intermediate formats in target dir (not cache).
 - Raise error if a requested output format out of several is not supported.
 - Support for @@frac and utf8 nearrow (extended texinfo.tex)
 - Predicates starting with $ (normally internal predicates) are now
   not documented by default. This allows, e.g., correct documentation
   of files using sr/bfall and others. Documentation can be forced
   with doinclude. Also applied to call_in_module/2 as special case.

### Fixed

 - Recovered inclusion of .txt figures in info (was not working).
 - Sometimes figures were not not being found by the texinfo backends.
   Fixed by running tex and conversion to pdf inside the cache dir.
 - Fixed spurious error message about no predicates being exported
   when there are in fact some exported via :- doc(doinclude, ...).
 - Correction in documentation of verbosity (all->full).
 - Fixed in documentation some dangling references to sections and
   wrong commands.
 - Fixed too large cartouches that do not fit on one page (changed
   filetype from part to documentation).
 - Fixed problem w/texinfo backend related to dotless i,j,etc.
  
### Improved

 - Better image selection and conversion for different formats.
 - Improvements documentation:
    - Mention use within the new IDEs supported (playground, VSC) in
      manual.
    - Improved the pointers to the documentation of the options
      available in SETTINGS files (doccfg) affecting manual
      generation.
    - Included additional mentioning of .md files and markdown.
    - Reduced some indentations that were generating spurious
      verbatims since due to markdown activation by default.
 - For large documents lpdoc was sometimes silent for relatively
   long periods. Improved by:
      - Changing some of the messages from verbose to progress.
      - Including also back-end messages in progress (and made them a
        bit shorter).
 - Better image conversion error messages.
 - Unified further the format of messages and improved texts.

### Changed

 - Not finding a figure gives warning instead of abort (and inserts
   figure name in doc). 


## [3.7.0] - 2024-2-16

### Added

 - Support common-changelog format for changelogs.
 - Document deployment of active logic documents (ADL)
 - Added `@@includecode@{Lang@}@{File@}` to lpdoc

### Fixed

 - Remove leading blank lines in code block env for non-html backends.

### Improved

 - Simplified usage of `@@includefact`, produces verbatim output.
 - Allow toplevel options in `lpdoc -T`.
 - Improved documentation of markdown syntax.
 - Improved documentation of runnable code blocks.
 - Better CSS colors in HTML backend for debugging marks.
 - Playground support code (HTML backend) loaded dynamically when
   runnable code blocks are detected.

### Changed

 - Changelog ported to markdown

## [3.6.0] - 2022-9-28

### Added

 - Merged CSS changes for playground and runnable code into `lpdoc.css`.
 - Basic JS functionality from playground moved to `lpdoc-aux.js`.
 - Theme button for website layout.
 - New `allow_runnable` configuration setting to enable interactive
   `code_runnable` code blocks (loading internally the playground).

### Improved

 - Allow `\\title` (or `@@title`) in plain file documentation entries (this
   allows specifying titles from `.lpdoc` or `.md` files in docstructure
   without need of auxiliary `.pl` files).
 - Included modern `texindex.awk` version.
 - Enable utf8 support by default in texinfo backend
   This backports utf8 support from modern `texinfo.tex` files into our 
   `texinfo.tex` version (until we support new `texinfo.tex` versions).
 - `info` output now auto-refreshes, for a single file in info format
   (`C-cDF` info; `C-cDB`) and viewing it in Emacs (`C-cDV`)
 - Added small note for escaping backslash characters inside strings
   in documentations correctly.
 - Nicer style for scrollbars, do not force vertical scrollbar anymore.

### Fixed

 - Do not escape @tt{``} and `''` in HTML strings.
 - Fix texinfo generation in single file doc mode.
   There was no top node and this produced a messed up index.
 - Fix error generating info for single file from Emacs (missing
   no_biblio option in default settings file).

### Changed

 - Introductory text included in first page if there is no summary
   (better for html).
 - File location (`find_file/3`) tries all possible extensions for each
   `vpath` instead of trying all `vpath`s for each possible extension.
 - Better heuristics for formatting individual `.lpdoc`/`.md`
   components (without `.pl` files).

## [3.5.0] - 2022-3-2

Highlights of this release:

 - ADDED: Dark color-scheme CSS.
 - IMPROVED: Selected test cases can be documented as examples
   (`example/1` prop).
 - IMPROVED: UTF-8 as default encoding in texinfo documentation.
 - IMPROVED: Added support for more accented characters in HTML
   backend.
 - IMPROVED: Simpler CSS (vars).

## [3.4.0] - 2021-3-18

Highlights of this release:

 - FIXED: Missing command declarations for markdown syntax.
 - NEW: `-op` for alternative code source suffix (useful for flycheck).
 - NEW: A `nil` backend for lpdoc syntax checking only (no
   actual documentation is generated).
 - FIXED: Avoid redefining @tt{SETTINGS} module errors.
 - FIXED: Improve support for `bibtex` accents.

## [3.3.0] - 2020-3-20

Highlights of this release:

 - Improved tolerance to missing dependencies, reporting disabled
   backends and causing dependency. Markdown translation turned on by
   default.

Detailed list of new features and changes:

 - ENHANCED: `is_operational` checks for backends (disable them
   dynamically if some 3rd-party dependencies, e.g., tex, makeinfo,
   etc. are missing). Informative messages about what is missing.
 - ENHANCED: Improved support for website generation
 - CHANGED: @tt{Up} navigation arrow links to `/` URL (i.e., website or
   doc index). This is handy when using @tt{M-x ciao-serve} or navigating
   the ciao-lang website.
 - CHANGED: Do not abort if `bibtex` is missing/failing (just emit
   error messages).
 - FIXED: Turn on markdown translation by default in default
   @tt{SETTINGS.pl} (used, e.g., by @tt{C-c D B} in the emacs mode).
 - FIXED: Check that arg 1 of fact pointed by @@includefact is a
   string.
 - FIXED: Dependency to `texindex` for dvi,ps,pdf formats.
 - FIXED: Do not abort doc generation on `convert` errors.


## [3.2.0] - 2018-12-06

 - Major improvements in HTML backend (which now is also mobile-friendly).
 - Built-in search in HTML manuals.
 - Do not generate `.ps` or `.dvi` by default.
 - (experimental) markdown-style documentation and documentation comments
   "%! ".

## [3.1.0] - 2016-12-31

 - Simplified manual setting files.
 - LPdoc can be used as a library (simplified interface
   `lpdoc(docmaker)`, etc.).
 - Adding built-in toplevel (in addition to the command-line
   interface).
 - The markup commands `@@uref@{URL@}` and
   `@@uref@{Text@}@{URL@}` have been deprecated. Use
   `@@href@{URL@}` and `@@href@{URL@}@{Text@}` instead.

## [3.0.0] - 2011-07-07

Major redesign of the documentation generator:

 - LPdoc redesigned to work internally with a 'doctree' representation
   (a-la Pillow). (Jose Morales)
 - A native HTML backend (not generated from texi). (Jose Morales)
 - Allow custom website generation from LPdoc documents.  (Jose
   Morales)
 - Two passes for document generation, allowing resolution of
   bibliographical references in all backends (including HTML). (Jose
   Morales)
 - `doc_structure/1` in @tt{SETTINGS} allows structure in LPdoc
   documents (sections can really be nested inside parts). (Jose
   Morales)
 - `:- doc(_,_)` is the recommended syntax for documentation
   comments now.
 - Replacing `:- comment` by `:- doc` in LPdoc code, updated
   documentation. (Jose Morales)

General improvements and bug fixes:

 - Designed a logo for LPdoc. (Jose Morales)
 - LPdoc comments can now be written using `%!`  style comment
   syntax. (Manuel Hermenegildo)
 - Now commas etc. are allowed in section names (so that they can be
   used in other formats). They are eliminated automatically in texi
   and info. This avoids wrong section names --and thus dangling
   pointers-- in generated texinfo files. (Manuel Hermenegildo)
 - Eliminated superfluous copy of summary in info mode.  (Manuel
   Hermenegildo)
 - Eliminated unsupported chars that broke texi manual
   cross-referencing. (Manuel Hermenegildo)
 - Improved treatment of accents (dotless i, dotless j, etc.).
   (Manuel Hermenegildo)
 - Initial size passed to `xdvi` more appropriate for current
   `xdvis`. (Manuel Hermenegildo)
 - Accents in bibliography fixed.  (Manuel Hermenegildo)
 - Now repeated sections are disambiguated. (Manuel Hermenegildo)
 - Eliminated unnecessary escaping (especially for &). (Manuel
   Hermenegildo)
 - Better detection of when version is not available. (Manuel
   Hermenegildo)
 - Added new `doc(address, _)` comment, which is the right place to
   put address/contact information in manuals (Jose Morales)
 - Added new `@@version@{@}` command (expands to the version of the
   software to be documented). (Jose Morales)
 - Shorter @tt{SETTINGS.pl} files (with some rudimentary,
   assertion-based checking of options) (Jose Morales)
 - Bug fix: `@@@@ include` and `@@@@ includeverbatim` are no
   longer a problem (space can be omitted) (Jose Morales)
 - Added and documented a new `documentation` filetype (for some
   parts of the manual that contains only documentation). That avoids
   the old trick of declaring a fake `main/0` predicate. (Jose
   Morales)
 - Style for subtitle added automatically (in texinfo, it is
   @em{emph}; in HTML it is normal text with smaller font). The
   entries in `subtitle_extra` are free-form. (Jose Morales)
 - Bugs and changelog appear now in the global links in the HTML
   backend. (Jose Morales)
 - Merged code that documented `.pl` and `.lpdoc` files. (Jose
   Morales)
 - No copyright section if no copyright comment. (Jose Morales)
 - Auxiliary documentation files ending in @tt{_doc} displayed
   incorrect names for the module (ending in @tt{_doc}). E.g.,
   `use_package(foo_doc)` was displayed instead of
   `use_package(foo_doc)`. Fixed. (Jose Morales)
 - In `verbatim` enviroments, new-line characters are removed from
   the beginning. (Jose Morales)
 - Fix wrong use of `erase/1` for clauses (which resulted in
   segmentation fault when documentation generation failed) (Jose
   Morales)
 - Fixed image generation (now uses `.png` files for HTML) (Jose
   Morales)
 - New code for text escape fixed some problems, like `@@/1`
   operator not being displayed correctly in Info. (Jose Morales)
 - Colors for Prolog variables (in HTML). (Jose Morales)
 - Added `@@begin@{alert@}` environment for alert messages (like
   cartouche, but in red). (Jose Morales)
 - Supporting `@@"` command for umlaut, in addition to `@@..`  (Jose
   Morales)
 - Double quotes correctly translated to HTML (Jose Morales)
 - `@@author` command to reference authors (changed `@@index` command
   referring to people by `@@author`, in all the documentation)
   (Jose Morales)
 - Simplification of documentation setting files (see the
   documentation for further details) (Jose Morales)
 - Using `open` for `lpdoc htmlview` command in MacOS X (Jose
   Morales)
 - Adding `html` and `pdf` formats as options for emacs
   customization of LPdoc (`html` is the default one now) (Jose
   Morales)
 - Improved detection of external tools for image conversion. (Manuel
   Hermenegildo)
 - Added section name syntax auto-correction. This avoids wrong
   section names --and thus dangling pointers-- in generated texinfo
   files. (Manuel Hermenegildo)
 - Document size more appropriate for current xdvi versions. (Manuel
   Hermenegildo)
 - Lpdoc no longer adds .info filename suffix to .infoindex entries
   since it breaks Debian's install-info --remove and goes against
   standard practice anyway. (Jose Luis Gonzalez)
 - Added option --cv, --comment-version, that tells lpdoc if the file
   has version comment. Formatting of lpdoc version comments
   completed. (Edison Mera)
 - Improved handling of option values. Added -d option to lpdoc, that
   allows defining additional values in the argument. Added options -l
   and -m that are similar to the corresponding lpmake options.
   (Edison Mera)

Support for in-code sections (experimental):

 - Latex-like font-lock highlight of sectioning documentation comments
   (`:- doc(C, "...")`, with @tt{C} one of `title`,
   `section`, and `subsection`).
 
   Currently the `section` and `subsection` comments are still
   ignored by LPdoc.  (Jose Morales)

Support for mathematical notation (experimental):

 - new `@@math@{...@}` and `@@begin@{displaymath@}...@@end@{displaymath@}`
   enviroments are supported (see the documentation for more details)
   (Jose Morales)
 - In documentation strings, single `\\` must be escaped
   (e.g. `@@math@{\\\\lambda@}`) (Jose Morales)
 - Supported in both the texinfo and HTML (using MathJax)
   backends. (Jose Morales)
 - Added `@@defmathcmd@{Cmd@}@{N@}@{Def@}` and
   `@@defmathcmd@{Cmd@}@{Def@}`, both for texinfo and HTML
   backends. Those LPdoc commands define new mathematical environments
   (equivalent to `\\newcommand`). (Jose Morales)

## [2.1.0] - 2004-10-28

Last version before moving to subversion. 1.9 and 2.0 were
merged. 1.9 (based on makefiles) is deprecated.

New functionality:

 - Use of `:- doc` declarations (as a shorthand for `comment`)
   now allowed.  (Manuel Hermenegildo)

 - Made xdvi viewer, ps viewer, and xdvi zoom size be parameters (the
   latter since new versions of xdvi display sizes differently than
   old ones).  (Manuel Hermenegildo)

 - Processing options can now be set for each file independently.
   (Manuel Hermenegildo)

 - Proper @concept{pdf generation} now achieved in most cases, thanks
   to newer versions of @apl{dvips}.  (Manuel Hermenegildo)

 - Added option -c Target in lpdoc, that treats Target as a separate
   component.  (Edison Mera)

 - Added option -f ConfigFile in lpdoc, that uses the file ConfigFile
   instead the default LPSETTINGS.pl.  (Edison Mera)

 - Added option ascii that generates documentation in ascii plain
   format.  (Edison Mera)

 - Added --help option. Is equal to -h.  (Edison Mera)

 - Added option testsettings to check that the settings file is
   correctly specified.  (Edison Mera)

 - Changed @pred{generate_html_pointer/5} by
   @pred{generate_html_pointer/6} to let it work with any given
   directory, and not only the working directory.  (Edison Mera)

## [2.0.36] - 2004-10-28

Updated to the recent changes of ciao, related to make package.  Use
of package make_new is no longer required.  (Edison Mera)

## [2.0.35] - 2004-10-28

Moved install options to the file installmkf.pl in ciao.  (Edison Mera)

## [2.0.33] - 2004-10-10

Added option testsettings.  It is to check that the settings file are
correctly specified.  (Edison Mera)

## [2.0.30] - 2004-10-08

Solved a bug in option texclean to let it work even if basemain is not
defined.  (Edison Mera)

## [2.0.29] - 2004-10-08

Added option ascii that generates documentation in ascii plain format.
(Edison Mera)

## [2.0.28] - 2004-10-08

Changed @pred{generate_html_pointer/5} by
@pred{generate_html_pointer/6} to let it work with any given
directory, and not only the working directory.  (Edison Mera)

## [2.0.27] - 2004-10-08

@pred{basemain/1} changed with @pred{_:basemain/1}.
@pred{_:startpage(StartPage)} changed with
@pred{get_value(_:startpage, StartPage)}.
@pred{_:papertype(PaperType)} changed with
@pred{get_value(_:papertype, PaperType)}.
@pred{_:htmlindex_headfile(HH)} changed with
@pred{get_value(_:htmlindex_headfile, HH)}.
@pred{_:htmlindex_tailfile(HT)} changed with
@pred{get_value(_:htmlindex_tailfile, HT)}.  @pred{libdir/1}
changed with @pred{_:libdir/1}.  @pred{make_directory/1} changed
with @pred{make_dirpath/1}.  @pred{docdir/1} changed by
@pred{_:docdir/1}.  @pred{delete_files/1} changed with
@pred{del_files_nofail/1}. (Edison Mera)

## [2.0.26] - 2004-10-08

Added option -c Target in lpdoc, that treats Target as a separate
component.  (Edison Mera)

## [2.0.25] - 2004-10-08

`use_module(library(TheAliasFile))` changed with
`use_module(TheAliasFile)`.  (Edison Mera)

## [2.0.24] - 2004-10-08

Added option -f ConfigFile in lpdoc, that uses the file ConfigFile
instead the default LPSETTINGS.pl.  (Edison Mera)

## [2.0.23] - 2004-10-08

When importing @pred{'../LPDOCSETTINGS'} now uses @pred{use_module/2}
to view the default configuration values.  (Edison Mera)

## [2.0.16] - 2004-02-05

Added --help option. Is equal to -h.  (Edison Mera)

## [2.0.10] - 2002-12-12

Proper @concept{pdf generation} now achieved in most cases, thanks to
newer versions of @apl{dvips}.  (Manuel Hermenegildo)

## [2.0.8] - 2001-11-28

Use of `:- doc` declarations (as a shorthand for `comment`) now
allowed.  (Manuel Hermenegildo)

## [2.0.7] - 2001-11-27

Made xdvi viewer, ps viewer, and xdvi zoom size be paramenters (the
latter since new versions of xdvi display sizes differently than old
ones).  (Manuel Hermenegildo)

## [2.0.6] - 2001-11-27

Fixed a few lingering CIAO to Ciao in documentation.  (Manuel
Hermenegildo)

## [2.0.3] - 1999-09-15

Processing options can now be set for each file independently.
(Manuel Hermenegildo)

## [2.0.0] - 1999-08-17

Major change to eliminate need for Makefiles: lpdoc is now a
standalone command (Manuel Hermenegildo). Proceeds in parallel with
further development of 1.9. Merge pending. Previous changes
incorporated since 1.8:

New functionality:

 - A new parameter @tt{PAPERTYPE} can be set in the @file{SETTINGS}
   file which controls the format of printed output.  (Manuel
   Hermenegildo)

 - Default @concept{pdf viewer} is now @apl{ghostview}, since recent
   versions handle `pdf` well.  (Manuel Hermenegildo)

 - Changed default style sheet in order to show HTML `pre` lines with a
   monospaced font.  (Daniel Cabeza Gras)

 - Mode definitions now documented in a separate section. The way they
   are documented has been improved.  (Manuel Hermenegildo)

 - References in files now updated only if `.refs` file is not
   empty.  (Manuel Hermenegildo)

 - A @em{copy} of the html style sheet is now included in
   @em{distributions}. Also @em{Copies} of the html and info index
   head and tail files. (Manuel Hermenegildo)

 - Made pointers relative in library html templates.  (Manuel
   Hermenegildo)

Bug fixes and other minor improvements:

 - Declarations now documented properly even if they have the same
   name and arity as a predicate.  (Manuel Hermenegildo)

 - Accented i's now translate correctly in html.  (Manuel
   Hermenegildo)

 - Fixed a funny installation quirk: while we want to install LPdoc in
   the Ciao group, the manuals produced by LPdoc should be installed
   in the LPdoc group.  (Manuel Hermenegildo)

 - Now using `lpdoclib` path alias.  (Manuel Hermenegildo)

 - Fixed bug in ordering of html indices in recent Linux versions,
   related to varying file listing order depending on locale.  (Manuel
   Hermenegildo)

## [1.9.56] - 2001-11-28

Use of `:- doc` declarations (as a shorthand for `comment`) now
allowed (needs compatible version of Ciao which has corresponding
modifications in assertion-processing lib).  (Manuel Hermenegildo)

## [1.9.55] - 2001-11-27

Made xdvi viewer, ps viewer, and xdvi zoom size be paramenters (the
latter since new versions of xdvi display sizes differently than old
ones).  (Manuel Hermenegildo)

## [1.9.54] - 2001-11-27

Fixed a few lingering CIAO to Ciao in documentation.  (Manuel
Hermenegildo)

## [1.9.53] - 2001-08-29

Minor bug in gmake infoview fixed.  (Manuel Hermenegildo)

## [1.9.52] - 2001-08-26

Fixed bug in ordering of html indices in recent Linux versions,
related to varying file listing order depending on
locale. Unfortunately, fix not complete yet. (Manuel Hermenegildo)

## [1.9.49] - 2000-04-16

Accented i's now translated correctly in html.  (Manuel Hermenegildo)

## [1.9.46] - 2000-03-17

Paths alias file now also read correctly for when generating
htmlindex.  (Manuel Hermenegildo)

## [1.9.45] - 2000-03-15

Now using `lpdoclib` path alias.  (Manuel Hermenegildo)

## [1.9.44] - 2000-03-01

A new parameter @tt{PAPERTYPE} can be set in the @file{SETTINGS} file
which controls the format of printed output.  (Manuel Hermenegildo)

## [1.9.43] - 2000-02-02

Default @concept{pdf viewer} is now @apl{ghostview}, sicne recent
versions handle `pdf` well.  (Manuel Hermenegildo)

## [1.9.41] - 1999-12-09

Fixed a funny installation quirk: while we want to install LPdoc in
the Ciao group, the manuals produced by LPdoc should be installed in
the LPdoc group.  (Manuel Hermenegildo)

## [1.9.29] - 1999-11-24

Changed default style sheet in order to show HTML `pre` lines with a
monospaced font.  (Daniel Cabeza Gras)

## [1.9.24] - 1999-11-23

Mode definitions now documented in a separate section. The way they
are documented has been improved.  (Manuel Hermenegildo)

## [1.9.23] - 1999-11-22

References in files now updated only if `.refs` file is not empty.
(Manuel Hermenegildo)

## [1.9.22] - 1999-11-22

Fixed bug in documentation text for declarations.  (Manuel
Hermenegildo)

## [1.9.21] - 1999-11-22

Declarations now documented properly even if they have the same name
and arity as a predicate.  (Manuel Hermenegildo)

## [1.9.20] - 1999-11-22

Fixed minor bug in lib Makefile which prevented full uninstallation.
(Manuel Hermenegildo)

## [1.9.11] - 1999-11-17

Minor changes to lib Makefile.  (Manuel Hermenegildo)

## [1.9.10] - 1999-11-15

Fixed bug that eliminated the html index files when generating html
manual.  (Manuel Hermenegildo)

## [1.9.9] - 1999-11-11

Default info index head and tail in lib is now generic.  (Manuel
Hermenegildo)

## [1.9.8] - 1999-11-07

Several improvements to makefiles including avoiding lpdoc being
affected by an upper definition of @tt{LIBDIR}.  (Manuel Hermenegildo)

## [1.9.7] - 1999-11-03

@em{Copies} of the html and info index head and tail files are now
included in @em{distributions}. This fixed a bug that cropped up
during installation of manuals.  (Manuel Hermenegildo)

## [1.9.6] - 1999-11-02

A @em{copy} of the html style sheet is now included in
@em{distributions}.  Fixes a bug that cropped up during installation
of manuals.  (Manuel Hermenegildo)

## [1.9.3] - 1999-07-14

Made pointers relative in library html templates.  (Manuel
Hermenegildo)

## [1.9.0] - 1999-07-08

In this release the name of the application has changed to
@apl{lpdoc}.

New commands:

 - @@begin@{cartouche@} and @@end@{cartouche@} commands
   now supported.
 - @@foonote command now supported.
 - New `gmake htmlview` command (makes a running @apl{netscape}
   visit the generated html manual). Suggested by Per Cederberg.
 - New `gmake distclean` command, intended for software
   distributions. Leaves the generated documents and eliminates
   @em{all} intermediate files (including `.texic`/`.texi`
   files).
 - Adobe `pdf` format now supported as a valid
   target. Unfortunately, embedded `.eps` figures are not supported
   at this time in pdf output.
 - The second argument of `:- comment(hide,...).` and `:- comment(doinclude,...).`
   declarations can now be a list of predicate names.
 - A `-u` @em{File} option is now supported so that a file
   including, e.g., path alias definitions can be included (this has
   the same functionality as the `-u` option in @apl{ciaoc}).
 - Now typing just `gmake` does nothing. In order to do something
   at least one target should be specified. This was necessary so that
   recursive invocations with empty arguments did nothing.
 - Added a new filetype: `part`. This allows splitting large
   documents into parts, each of which groups a series of chapters.

Other new functionality:

 - A style sheet can now be specified which allows modifying many
   characteristics of the html output (fonts, colors, background, ...)
   (thanks to Per Cederberg).
 - Added limited support for changing page numbering (in
   @file{SETTINGS} file).
 - The concept indexing commands (@@index, @@cindex, and @@concept)
   now work somewhat differently, to make them consistent with other
   indexing commands.
 - The old @em{usage} index is now called, more appropriately,
   @em{global} index. Correspondingly, changed things so that now
   every definition goes to the global index in addition to its
   definitional index.
 - Imported files from module `user` are now documented
   separately.
 - Now a warning is issued if characters unsupported by info are used
   in section names.
 - Navigation in html docs was improved.
 - The table of contents in printed manuals now contains entries for
   the individual descriptions of predicates, props, regtypes,
   declarations, etc.  This can be shut off with the `-shorttoc`
   option.
 - Made more silent in normal conditions: file inclusion is
   muted now unless `-v` option is selected.
 - A single `.texi` file is now constructed (by grouping the
   `.texic` files generated for all components) in which the
   references and menus are resolved. This has the advantage that the
   process of resolving references and menus has now been sped up very
   significantly.  Also, `texi` is now a valid target (perhaps
   useful for distributions). The generated files now have `texic`
   (@em{texinfo component}).
 - Now, declarations are always documented as long as there is a
   `decl` assertion.  Also, they are now documented in a separate
   section.

Bug fixes and other minor improvements:

 - The directory containing html manual is now called
   @em{BASENAME}@tt{_html} instead of just @em{BASENAME}, which was
   confusing.
 - Now requesting building a .ps only does not leave a .dvi behind
   (useful for distributions).
 - File names can now include the symbol @tt{_} even if they contain
   figures.
 - @apl{TeX}-related intermediate files are now cleaned up after each
   run in order to avoid clutter.
 - Fixed `-modes`, which was broken since going to the new
   normalizer (was normalizer problem). Fixed problem with no
   documentation when only modes given.
 - Fixed duplication of documentation for internal predicates when
   also exported.
 - Minor formatting problem when no documentation nor definition found
   for a regtype fixed.
 - Determining exports, imports, etc. now done solely by calls to
   @lib{c_itf} library (and, thus, synchronized with @apl{ciaoc}
   compiler).

(Manuel Hermenegildo)

## [1.8.42] - 1999-06-30

Changed texi2html to generate better HTML with attributes for styling
(instead of <font> tags). (Per Cederberg)

## [1.8.41] - 1999-06-22

Added `gmake htmlview` command (makes a running @apl{netscape}
visit the generated html manual). Suggested by Per Cederberg. (Manuel
Hermenegildo)

## [1.8.40] - 1999-06-17

File names can now include the symbol @tt{_} even if they contain
figures (fixes a reported bug).  (Manuel Hermenegildo)

## [1.8.39] - 1999-06-10

Added default style sheet for html and introduced conditional copying.
(Per Cederberg)

## [1.8.38] - 1999-06-09

Now using style sheets for html output.  (Per Cederberg)

## [1.8.36] - 1999-05-27

Directory containing html manual is now called @em{BASENAME}@tt{_html}
instead of just @em{BASENAME}, which was confusing.  (Manuel
Hermenegildo)

## [1.8.28] - 1999-04-21

Eliminated unnecessary loading of `iso_byte_char` and `dcg_expansion`.
(Manuel Hermenegildo)

## [1.8.23] - 1999-04-08

A `-u` @em{File} option is now supported so that a file including
path alias definitions can be included (this has the same
functionality as the `-u` option in @apl{ciaoc}).  (Manuel
Hermenegildo)

## [1.8.22] - 1999-04-08

A background can now be specified for the `html` manuals (needs a
modified version of @apl{texi2html} included with the @apl{lpdoc}
library).  (Manuel Hermenegildo)

## [1.8.21] - 1999-04-07

@apl{TeX}-related intermediate files are now cleaned up after each run
in order to avoid clutter.  (Manuel Hermenegildo)

## [1.8.20] - 1999-04-07

Adobe `pdf` format now supported as a valid target. Unfortunately,
embedded `.eps` figures are not supported at this time in pdf
output. (Manuel Hermenegildo)

## [1.8.19] - 1999-04-07

A single `.texi` file is now constructed (by grouping the
`.texic` files generated for all components) in which the
references and menus are resolved. This has the advantage that the
process of resolving references and menus has now been sped up very
significantly.  Also, `texi` is now a valid target (perhaps useful
for distributions).  (Manuel Hermenegildo)

## [1.8.17] - 1999-04-05

Now requesting building a .ps only does not leave a .dvi behind
(useful for distributions).  (Manuel Hermenegildo)

## [1.8.3] - 1999-03-26

Changed @tt{Makefile.skel} so that now typing just `gmake` does
nothing. In order to do something at least one target should be
specified. This was necessary so that recursive invocations with empty
arguments did nothing.  (Manuel Hermenegildo)

## [1.8.1] - 1999-03-25

New `gmake distclean`, intended for software distributions. Leaves
the generated documents and eliminates @em{all} intermediate files
(including `.texi` files). (Manuel Hermenegildo)

## [1.8.0] - 1999-03-24

This version completes the port to using the ciao 0.8 modular
assertion processing library. In addition, it includes the
following improvements:

 - Now, if the name of a file being documented ends in @tt{_doc}, the
   @tt{_doc} part is left out when referring to the file in the
   documentation (useful if one would like to place the documentation
   declarations in different file).
 - It is now possible to declare (via a @decl{comment/2} declaration)
   the intended use of a file which is not a module (i.e. a package,
   user, or include file), which results in correct documentation of
   operator definitions, new declarations, etc. The declaration is
   only needed for 'user' files (i.e., files to be loaded with
   @pred{ensure_loaded/1}).
 - Separated generation of the manuals from their installation. I.e.,
   `gmake install` now does not force a `gmake all`, which has
   to be done by hand. This was necessary to ensure correct
   installation of distributed manuals, even if modification dates are
   changed during installation. Previously, in some cases generation
   was triggered unnecessarily.
 - New `-v` option allows using quieter by default operation when
   not debugging.
 - New option `-propmods` makes the name of the module in which a
   property is defined appear in front of the property in the places
   where the property is used.
 - New option `-noisoline` makes the textual explanation of the
   @prop{iso/1} property not appear in the description of the usage
   (but the @iso symbol does appear)
 - Two new options, `-nosysmods` and `-noengmods`, selectively
   avoid listing the system or engine libraries used.
 - If there is no declaration for a predicate, now a line is output
   with the name and arity and a simple comment saying that there is
   no further documentation available (this has the great advantage
   that then it goes in the index, and, for example in ciao, they get
   added to completion commands!).
 - Now, if a property or regtype declaration has no textual comment,
   the actual definition is given (first level only) in the place
   where it is documented, and a simple generic message where it is
   used.
 - Added @@noindent and @@iso commands.
 - Nicer spacing now when printing predicate names which are
   operators, as well as modes, etc.
 - Reporting of versions in libraries has been improved: now both the
   global version and the last version in which the library itself was
   changed are reported.
 - Exported new declarations also documented now for include-type
   files.
 - A module is now documented even if exports nothing at all.
 - Engine modules used now documented even if no other modules used
   (was a reported bug).
 - Fixed indexing of names containing @@ etc. for newer versions of
   texinfo.
 - Tabs in verbatim modes now converted to a number of spaces (8). Not
   perfect, but produces better output than leaving the tabs in.
 - Tex is now run in 'nonstopmode' which means it will typically not
   stop if there are minor errors (but some errors may go
   unnoticed...).
 - The full path of the version maintenance directory is now computed
   (correctly) using the directory of the `.pl` file being
   documented as base.
 - Notices for missing subtitle, copyright, and summary now only given
   from main file and not for components.
 - Added special handling of regtype and generalized it to handle some
  props specially if there is a certain comp property present.

(Manuel Hermenegildo)

## [1.7.34] - 1999-03-22

In @file{Makefile.skel}, separated generation of the manuals from
their installation. I.e., `gmake install` now does not force a
`gmake all`, which has to be done by hand. This was necessary to
ensure correct installation of distributed manuals, even if
modification dates are changed during installation. Previously, in
some cases generation was triggered unnecessarily.  (Manuel
Hermenegildo)

## [1.7.14] - 1999-03-01

Minor fix to dependencies in Makefile.  (Manuel Hermenegildo)

## [1.7.0] - 1998-12-02

Major port to use the ciao 0.8 modular assertion processing library.
(Manuel Hermenegildo)

## [1.6.9] - 1998-09-16

Added @tt{DOTcshrc} files to library.  (Manuel Hermenegildo)

## [1.6.8] - 1998-09-16

Updated documentation. Separated intro stuff into chapters: more
logical, faster compilation.  (Manuel Hermenegildo)

## [1.6.7] - 1998-09-15

Nicely formatted @tt{README} and @tt{INSTALL} files now generated
automatically on installation.  (Manuel Hermenegildo)

## [1.6.4] - 1998-09-11

Trouble shooting section added to documentation.  (Manuel
Hermenegildo)

## [1.6.0] - 1998-09-08

Added support for @concept{inserting images} (.eps files) in text
via @@image command, @concept{email addresses} via @@email command,
and @concept{url references} via @@uref command.

Unix 'man' output much improved. Also, it now includes a
@concept{usage section}. The correspoding text must be given in a
string contained in the first argument of a fact of the
@pred{usage_message/1} predicate which appears in the
program. Also, formatting of 'man' pages has been greatly improved.

A new 'ascii' format is now supported: a simple minded ascii manual
(basically, an info file without pointers).

(Manuel Hermenegildo)

## [1.5.6] - 1998-08-31

@tt{Makefile} can now use printf instead of echo for portability
(unfortunately, different versions of echo behave very differently).
(Manuel Hermenegildo)

## [1.5.0] - 1998-08-23

Now supporting a `@@cite` command (YES!). It automatically accesses the
bib entries in `.bib` files (using @apl{bibtex}) and produces a
'References' appendix. @@cite can be used in the text strings
exactly as `\\cite` in LaTeX. The set of bib files to be used is
given in the @tt{SETTINGS} file.

Defining the type of version maintenance that should be performed
by the @apl{emacs} ciao.el mode (i.e., whether version numbers are
in a given directory or in the file itself) is controlled now via a
standard @decl{commment/2} declaration. You should now write a
declaration such as:
```
:- comment(version_maintenance,dir('../version')).
```
to state that control info is kept in directory
`../version`. This has the advantage that it is shorter than the
previous solution and that lpdoc can read this info easily. Using
this guarantees that the version numbers of the manuals always
concide with those of the software.

Generation of indices of manuals (.htmlbullet files): if several
manuals are installed in the same directory, an index to them is
now generated at the beginning of the html cover page describing
the directory.

(Manuel Hermenegildo)

## [1.4.5] - 1998-08-14

More improvements to @tt{Makefile}. (Manuel Hermenegildo)

## [1.4.3] - 1998-08-07

Several changes in @tt{Makefile} (Manuel Hermenegildo)

## [1.4.0] - 1998-08-04

The set of paths defined in @tt{SETTINGS} for finding the source files
are now also used to find 'included' files. As a result, full path
is not needed any more in, e.g, @@include command.

New @@ref command which can be used to refer to chapeter, sections,
subsections, etc..

Support for recent minor changes in assertion format, including '#'
as comment separator.

Used modules are now separated in documentation (in the interface
description) by type (user, system, engine...).

Supports new 'hide' option in comments, to prevent an exported
predicate from being documented. This is useful for example for
avoiding mentioning in the documentation multifile predicates which
are not intended to be modified by the user.

(Manuel Hermenegildo)

## [1.3.2] - 1998-07-10

Added overall handling of -s option -- separating system and user
libraries.  (Manuel Hermenegildo)

## [1.3.0] - 1998-07-10

Exports are now listed in the chapter header separated by kind
(pred, types, properties, ...).

The list of other modules used by a module is now separated in the
chapter header into User and System modules (controlled by two sets
of paths in @tt{SETTINGS}).

New @em{hide} option of comment/2 decl prevents an exported
predicate from being included in the documentation:
`:- comment(hide,p/3)`.

(Manuel Hermenegildo)

## [1.2.1] - 1998-06-10

More improvements to documentation.  (Manuel Hermenegildo)

## [1.2.0] - 1998-06-04

Major overall improvements...  (Manuel Hermenegildo)

## [1.1.26] - 1998-05-23

Links now preserved when making distribution tar files.  (Manuel
Hermenegildo)

## [1.1.24] - 1998-05-06

Improved makefiles (no explicit calls to gmake).  (Manuel Hermenegildo)

## [1.1.8] - 1998-4-9

Enhanced documentation.  (Manuel Hermenegildo)

## [1.1.3] - 1998-4-7

Fixed document makefile so that no formatting is repeated (by creating
.ltxi files).  (Manuel Hermenegildo)

## [1.1.2] - 1998-4-6

Replaced `atom_chars/2` with `atom_codes/2` (to make code more
compatible with older Prologs).  (Manuel Hermenegildo)

## [1.1.1] - 1998-4-3

Added treatment of `comment` command.  (Manuel Hermenegildo)

## [1.1.0] - 1998-3-31

Incorporated autodoc and autodoformats library to source in order to
make distribution standalone. Improvements to installation and
documentation. @tt{Makefile}s now also install documentation in public
areas and produce global indices. Several documents can cohexist in
the same installation directory. (Manuel Hermenegildo)

## [1.0.6] - 1998-3-30

Improved installation.  (Manuel Hermenegildo)

## [1.0.5] - 1998-3-19

Minor changes to Doc@tt{Makefile} to fix some bugs.  (Manuel Hermenegildo)

## [1.0.4] - 1998-3-11

Enhaced documentation.  (Manuel Hermenegildo)

## [1.0.3] - 1998-2-26

Fixed minor bug in @tt{Makefile}s.  (Manuel Hermenegildo)

## [1.0.2] - 1998-2-25

Added version reporting at startup.  (Manuel Hermenegildo)

## [1.0.1] - 1998-2-25

Made major fixes to makefiles to improve installation and
deinstallation.  (Manuel Hermenegildo)

## [1.0.0] - 1998-2-24

First Ciao-native distribution, with installation. (Manuel
Hermenegildo)

## [0.9.1] - 1998-2-24

File version is now global lpdoc version.  (Manuel Hermenegildo)

## [0.9.0] - 1998-2-24

Intermediate version, preparing for first major release. Modified
@tt{Makefile} and @tt{SETTINGS} to handle installation of
manuals. (Manuel Hermenegildo)

## [0.6.1] - 1998-2-20

Modified @tt{Makefile} and @tt{SETTINGS} to handle installation of
manuals. (Manuel Hermenegildo)

## [0.6.0] - 1998-2-10

Added new indices and options, as well as more orthogonal handling of
files. (Manuel Hermenegildo)

## [0.4.0] - 1998-2-24

Added support for `nroff -m` formatting (e.g., for man pages). Added
support for optional selection of indices to be generated. Added
support for reexported predicates. Added (low level) ascii
format. Added option handling (`-nobugs` `-noauthors` `-noversion`
`-nochangelog` `-nopatches` `-modes` and `-headprops` ...).
`-literalprops`. Fixed presentation when there are multiple kinds of
assertions. Better error checking for includefact/includedef. (Manuel
Hermenegildo)

## [0.3.0] - 1998-2-10

Changed file reader to use Ciao native builtins. As a result, syntax
files and full Ciao syntax now supported. Major reorganization of the
code to make formatting more orthogonal. Now applications and
libraries can be components or main files, standalone or with
components interchangeably. @@includefact, new predicate types, used
libraries now precisely detected, `docinclude` option. (Manuel
Hermenegildo)

## [0.2.1] - 1998-1-26

Changed file reader to use Ciao native builtins. As a result, syntax
files and full Ciao syntax now supported. (Manuel Hermenegildo)

## [0.2.0] - 1997-12-16

Ported to native ciao.  Version handling, selection of indices,
@@include. Added generation of an html brief description for a global
index. Added unix manual page generation. Added support for specifying
library paths. `-l` option for htmlindex and man. Installation improved:
now all files for one application in the same directory. (Manuel
Hermenegildo)

## [0.1.9] - 1997-11-10

Added handling of `-l` option for htmlindex and man. (Manuel
Hermenegildo)

## [0.1.8] - 1997-9-19

Changed argument parsing to use DCGs -- much nicer! (Manuel
Hermenegildo)

## [0.1.7] - 1997-9-19

Added support for specifying library paths. (Manuel Hermenegildo)

## [0.1.6] - 1997-9-16

Using new version of autodoc library. (Manuel Hermenegildo)

## [0.1.5] - 1997-9-15

Modified towards making it run under both SICStus and Ciao
native. (Manuel Hermenegildo)

## [0.1.4] - 1997-8-20

Version number automatically included in files if available.

## [0.1.3] - 1997-8-12

Added index option handling.

## [0.1.2] - 1997-8-11

Installation improved: now all files for one application in the same
directory.

## [0.1.1] - 1997-8-07

Changed installation method to include automatic handling of version
numbers.

## [0.1.0] - 1997-07-30

First official version (major rewrite from several previous
prototypes, autodocumented!). (Manuel Hermenegildo)

## [0.0.0] - 1996-10-10

First prototype.
