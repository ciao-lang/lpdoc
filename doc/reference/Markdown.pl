:- use_package([assertions]).

% :- doc(filetype, documentation).
:- doc(filetype, part).

:- doc(title, "Documentation markdown language").

:- doc(author,"The Ciao Development Team").

:- doc(module,"

This section describes the @em{markdown}-flavor lightweight syntax for
writing documentation. @apl{LPdoc} supports combining both markup and
markdown syntax in the same document. Internally, all markdown syntax
is translated to the equivalent markup before processing.

@section{Syntax}

@subsection{Sections}

Sections are marked with one or more `#` symbols at the beginning of
a line:

```
# Sections and subsections

Text for the section.

## Subsection

Text for the subsection.

### Subsubsection

Text for the subsubsection.
```

@subsection{Styling text}

Text surrounded by @tt{_} (or @tt{*}), @tt{__} (or @tt{**}) produces emphasized or bold text:
```
  Text can be _emphasized_ (*emphasized*) or __bold__ (**bold**).
```

produces the following results:

@begin{note}
Text can be _emphasized_ (*emphasized*) or __bold__ (**bold**).
@end{note}

@subsection{Paragraphs}

Paragraphs are break by blank lines, as follows:
```
Begin of
a paragraph.
This paragraph ends here.

Begin of a new paragraph.
This paragraph ends here.

The last new paragraph. This paragraph ends here.
```

which produce the following results:

@begin{note}
Begin of
a paragraph.
This paragraph ends here.

Begin of a new paragraph.
This paragraph ends here.

The last new paragraph. This paragraph ends here.
@end{note}

@subsection{Lists}

Lists are composed of lines beginning with @tt{-} items, which can be
nested:

```
  - item 1
    - subitem 1-1
    - subitem 1-2
      - subitem 1-2-1
    - subitem 1-3
  - item 2
    - subitem 2-1
  - item 3
```

produces the following result:

@begin{note}
  - item 1
    - subitem 1-1
    - subitem 1-2
      - subitem 1-2-1
    - subitem 1-3
  - item 2
    - subitem 2-1
  - item 3
@end{note}

Additionally you can write *enumerated lists*:

```
Enumerated list with automatic numbering:
  -# First
  -# Second
  -# Third

Enumerated list with explicit numbering:
  1. First
  2. Second
  3. Third

Enumerated list with non-consecutive explicit numbering:
  2. First
  4. Second
  6. Third

Enumerated list mixing all above:

 2. First
 4. Second
 -# Third
 -# Fourth
 10. Fifth
 -# Sixth
```

producing:

@begin{note}
Enumerated list with automatic numbering:
  -# First
  -# Second
  -# Third

Enumerated list with explicit numbering:
  1. First
  2. Second
  3. Third

Enumerated list with non-consecutive explicit numbering:
  2. First
  4. Second
  6. Third

Enumerated list mixing all above:

 2. First
 4. Second
 -# Third
 -# Fourth
 10. Fifth
 -# Sixth
@end{note}

Description lists are used to describe some denoted elements:

```
  - opt :: First
  - foo :: Second
  - bar :: Third
```

which produces:

@begin{note}
  - opt :: First
  - foo :: Second
  - bar :: Third
@end{note}

Description lists may contain richer elements as items:

``` 
  - `atom` :: an atom.
  - `append/3` :: a predicate or functor name.
  - `f(X0,...,Xn)` :: some term with variables `X0`, ..., `Xn`.
  - `X` :: a variable.
  - $x^2$ :: some math.
  - $\bigwedge_j f_j(x_0, \ldots, x_n)$ :: some complex math.
``` 

which produces:

@begin{note}
  - `atom` :: an atom.
  - `append/3` :: a predicate or functor name.
  - `f(X0,...,Xn)` :: some term with variables `X0`, ..., `Xn`.
  - `X` :: a variable.
  - $x^2$ :: some math.
  - $\bigwedge_j f_j(x_0, \ldots, x_n)$ :: some complex math.
@end{note}

@subsubsection{Links (markdown style)}

You can write links in markdown style:

```
A link to \\href{https://ciao-lang.org} showing its URL.

A link to [Ciao](https://ciao-lang.org) hiding its URL.

A link to [The **Ciao** System](https://ciao-lang.org) hiding its URL
with a complex string.
```

which produces:

@begin{note}
A link to \\href{https://ciao-lang.org} showing its URL.

A link to [Ciao](https://ciao-lang.org) hiding its URL.

A link to [The **Ciao** System](https://ciao-lang.org) hiding its URL
with a complex string.
@end{note}

Org-mode style links are also suppored:

```
A link to [[https://ciao-lang.org]] showing its URL.

A link to [[https://ciao-lang.org][Ciao]] hiding its URL.

A link to [[https://ciao-lang.org][The **Ciao** System]] hiding its URL
with a complex string.
```

@begin{note}
A link to [[https://ciao-lang.org]] showing its URL.

A link to [[https://ciao-lang.org][Ciao]] hiding its URL.

A link to [[https://ciao-lang.org][The **Ciao** System]] hiding its URL
with a complex string.
@end{note}

@subsubsection{Code spans and code blocks}

In addition to the @tt{\\@tt\\{\\}} markup environments, we support
code spans with a single backtick, as follows:

```
This is a predicate name `append/3`, a variable name `X`, an
atom name `foo`, a quoted atom name `'foo'`.
```

producing:

@begin{note}
This is a predicate name `append/3`, a variable name `X`, an
atom name `foo`, a quoted atom name `'foo'`.
@end{note}

Blocks of code are marked with triple backticks (you may use `~~~` to
escape @tt{```}, if your code contains that sequence of backtick
characters):

```
Text that is 4-char indented is recognized as code:

    list([]).
    list([X|Xs]) :- list(Xs)

```

which produces:

@begin{note}
Text that is 4-char indented is recognized as code:

    list([]).
    list([X|Xs]) :- list(Xs)
@end{note}

Code blocks also support (optional) language specifier to enable
syntax coloring and other special features (like @index{runnable} code
blocks):

~~~
```ciao
list([]).
list([X|Xs]) :- list(Xs)
```
~~~

which produces:

```ciao
list([]).
list([X|Xs]) :- list(Xs)
```
").
