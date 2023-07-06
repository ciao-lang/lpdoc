:- use_package([assertions]).

% :- doc(filetype, documentation).
:- doc(filetype, part).

:- doc(title, "Including Editable and Runnable Examples").

:- doc(author,"The Ciao Development Team").

:- doc(module,"

This section describes how to include editable and runnable code
blocks in @apl{LPdoc} documents with an @em{embedded} playground, to
create interactive @em{Active Logic Documents} (@apl{ALD}s). 

@section{Runnable Code Blocks}

A @em{runnable} code block is a special documentation code block where
the language is marked as @tt{ciao_runnable}, as follows:

~~~
```ciao_runnable
...
```
~~~

These code fragments are automatically rendered as editable cells that
can be run in place or loaded into a separate playground. Additional
marks within the code provides different behaviors and functionality.
Below, we provide an overview of the available modes along with a
brief explanation for each:

@begin{itemize}

@item @bf{Focus}: The @tt{\\@begin\\{focus\\}} and
  @tt{\\@end\\{focus\\}} directives are used to selectively show
  certain sections of the code, while hiding the rest (typically
  module declarations, imports, or auxiliary code, tests, etc.).

  To reload or reset the code in this cell, simply @bf{click on the
  green tick mark} located in the top left corner of the pane. This
  functionality enables you to reload the code at any time.

  Example:
~~~
```ciao_runnable
:- module(_, [qsort/2], []).
%! \\begin{focus}
qsort(Data, Out) :-
    qsort_(Data, Out, []).

qsort_([], R, R).
qsort_([X|L], R, R0) :-
    partition(L, X, L1, L2),
    qsort_(L2, R1, R0),
    qsort_(L1, R, [X|R1]).
%! \\end{focus}

partition([],_,[],[]).
partition([X|L],Y,[X|L1],L2) :-
    X =< Y, !,
    partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
    partition(L,Y,L1,L2).
```
~~~

@item @bf{Queries}: A query cell is specified by a runnable code block
  where the code begins with `?-`. Example:

~~~
```ciao_runnable
?- qsort([1,5,4,2,3],X).
```
~~~

@item Interactive @bf{exercises}: In this interactive cell, you can
  engage in exercises by editing the code and adding comments, which
  serve as hints or instructions. Alternatively, the description of
  the exercise may be provided in the surrounding text. By clicking on
  the @bf{yellow face} icon, the code undergoes evaluation, e.g.,
  including the execution of hidden unit tests, which provide feedback
  to the reader.

@comment{If the reader wishes to give up and view the solution, they can
  request it, and the proposed solution will be displayed.}

  Segments enclosed within hint directives function similarly to focus
  segments, offering hints or instructions. However, if the solution
  is requested, the hint segment will be replaced with the
  corresponding solution, indicated by the solution directives.

  Example:
~~~
```ciao_runnable
%! \\begin{hint}
Proposed exercise
%! \\end{hint}

(Optional) Unit tests

%! \\begin{solution}
Solution
%! \\end{solution}
```
~~~

@end{itemize}

@comment{@item (experimental) @bf{miniplayground}: A miniplayground can be
  displayed, offering a simplified version of the playground. It
  retains the structure of the playground but includes only the
  debugger feature, omitting other functionalities.}

The `ciao_runnable` language tag also allows the following additional
annotations to interact with the browser using Javascript and/or
Prolog code running through WebAssembly.

@begin{alert}
The following are experimental features whose API may change in future
versions. Usage requires knowledge of HTML, Javascript (and some
internals of `ciao_playground.js`).
@end{alert}

@begin{itemize}

@item @bf{jseval}: Escape mechanism to allow execution of arbitrary
  Javascript code. The text within `jseval` is executed at document
  initialization (following the sequential order of playground cells)
  as a Javascript asynchronous function. Typically this is done as:
~~~
```ciao_runnable
%! \\begin{jseval}
async() => { <<JSCODE>> }
%! \\end{jseval}
```
~~~

Example (when included, the HTML document will turn upside-down):
~~~
```ciao_runnable
%! \\begin{jseval}
async() => { document.body.style.transform = \"rotate(180deg);\" }
%! \\end{jseval}
```
~~~

@item @bf{dynpreview}:
  Allow dynamically generated HTML fragments. See for example
  `catalog_ui/bundles_dyn.pl` in the website bundle. It requires a
  JSON structure that is passed to `setup_dynpreview` (see
  `ciao_playground.js`). For example:

~~~
```ciao_runnable
%! \\begin{dynpreview}
{
  render_pred: 'bundles_dyn:render',
  state_hash: true,
  depends: ['ciaowasm', 'website', 'lpdoc'],
  on_init: ['use_module(catalog_ui(bundles_dyn))']
}
%! \\end{dynpreview}
```
~~~

The `render_pred` field specifies the predicate that will generate the
HTML contents (as a string, e.g., using @lib{pillow}). The
`state_hash` field specifies if the URL hash is passed as parameter
(e.g., to maintain a state). The rest of fields indicate the bundle
dependencies and initialization goals.

@end{itemize}

@section{Examples}

The following example shows a programming task with some initial
hints, one valid solution (there may be others), a some set of tests
to validate the user code:

~~~
```ciao_runnable
:- module(_, _, [assertions]).

:- test factorial(A, B) : (A = 0) => (B = 1) + (not_fails, is_det).
:- test factorial(A, B) : (A = 1) => (B = 1) + (not_fails, is_det).
:- test factorial(A, B) : (A = 2) => (B = 2) + (not_fails, is_det).
:- test factorial(A, B) : (A = 3) => (B = 6) + (not_fails, is_det).
:- test factorial(A, B) : (A = 4) => (B = 24) + (not_fails, is_det).
:- test factorial(A, B) : (A = 5) => (B = 120) + (not_fails, is_det).
:- test factorial(A, B) : (A = 0, B = 0) + (fails, is_det).
:- test factorial(A, B) : (A = 5, B = 125) + (fails, is_det).
:- test factorial(A, B) : (A = -1) + (fails, is_det).

%! \\begin{hint}
% TASK 1 - Rewrite with Prolog arithmetic 
factorial(0,s(0)).    % TODO: Replace s(0) by 1
factorial(M,F) :-     % TODO: Make sure that M > 0
    M = s(N),         % TODO: Compute N from M using is/2 (note that N is 
    factorial(N,F1),  %       unbound, so you need to compute N from M!)
    times(M,F1,F).    % TODO: Replace times/3 by a call to is/2 (using *)
% When you are done, press the circle (\"Run tests\") or the arrow 
% (\"Load into playground\").
%! \\end{hint}

%! \\begin{solution}
factorial(0,1). 
factorial(N,F) :-
    N > 0,
    N1 is N-1,
    factorial(N1,F1),
    F is F1*N.
%! \\end{solution}
```
~~~

This produces the following cell:

```ciao_runnable
:- module(_, _, [assertions]).

:- test factorial(A, B) : (A = 0) => (B = 1) + (not_fails, is_det).
:- test factorial(A, B) : (A = 1) => (B = 1) + (not_fails, is_det).
:- test factorial(A, B) : (A = 2) => (B = 2) + (not_fails, is_det).
:- test factorial(A, B) : (A = 3) => (B = 6) + (not_fails, is_det).
:- test factorial(A, B) : (A = 4) => (B = 24) + (not_fails, is_det).
:- test factorial(A, B) : (A = 5) => (B = 120) + (not_fails, is_det).
:- test factorial(A, B) : (A = 0, B = 0) + (fails, is_det).
:- test factorial(A, B) : (A = 5, B = 125) + (fails, is_det).
:- test factorial(A, B) : (A = -1) + (fails, is_det).

%! \\begin{hint}
% TASK 1 - Rewrite with Prolog arithmetic 
factorial(0,s(0)).    % TODO: Replace s(0) by 1
factorial(M,F) :-     % TODO: Make sure that M > 0
    M = s(N),         % TODO: Compute N from M using is/2 (note that N is 
    factorial(N,F1),  %       unbound, so you need to compute N from M!)
    times(M,F1,F).    % TODO: Replace times/3 by a call to is/2 (using *)
% When you are done, press the circle (\"Run tests\") or the arrow
% (\"Load into playground\").
%! \\end{hint}

%! \\begin{solution}
factorial(0,1). 
factorial(N,F) :-
    N > 0,
    N1 is N-1,
    factorial(N1,F1),
    F is F1*N.
%! \\end{solution}
```

As shown above, tests can be included, hints and solutions provided,
etc.

It is also possible to specify that only some parts of the code be
shown by placing those parts between begin focus and end focus
directives. For example:

~~~
```ciao_runnable
:- module(_, _, [assertions,sr/bfall]).
%! \\begin{focus}
factorial(0,s(0)).
factorial(s(N),F) :-
    factorial(N,F1),
    times(s(N),F1,F).
%! \\end{focus}

nat_num(0).
nat_num(s(X)) :- nat_num(X).

times(0,Y,0) :- nat_num(Y).
times(s(X),Y,Z) :- plus(W,Y,Z), times(X,Y,W).

plus(0,Y,Y) :- nat_num(Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).
```
~~~

results in:

```ciao_runnable
:- module(_, _, [assertions,sr/bfall]).
%! \\begin{focus}
factorial(0,s(0)).
factorial(s(N),F) :-
    factorial(N,F1),
    times(s(N),F1,F).
%! \\end{focus}

nat_num(0).
nat_num(s(X)) :- nat_num(X).

times(0,Y,0) :- nat_num(Y).
times(s(X),Y,Z) :- plus(W,Y,Z), times(X,Y,W).

plus(0,Y,Y) :- nat_num(Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).
```

Programs can be modules or 'user' (i.e., non-modular) code. The focus
facility can be used as shown above to select whether boilerplate
lines (such as, e.g., module declarations, imports, auxiliary code,
etc.) are shown in the output or not.

Finally, runnable and editable queries can also be easily defined as
follows:

~~~
```ciao_runnable
?- factorial(X,s(s(s(s(s(s(0))))))).
```
~~~

resulting in: 

```ciao_runnable 
?- factorial(X,s(s(s(s(s(s(0))))))).
``` 

There is essentially one toplevel per page; all programs in @apl{ALD}
are loaded into this toplevel and all queries in the page are executed
in that toplevel, against all the code (possibly separate modules)
that has been loaded into the toplevel at that point.

@section{Full page example}

The following pointers provide a complete example of a class exercise
that uses embedded code, showing the full source and the full output:

@begin{itemize}
@item This is the [source of the page (written in this case in markdown)](/ciao/build/doc/lpdoc.html/factorial_peano_iso_source.html)
@item and this is the [ALD produced by LPdoc](/ciao/build/doc/lpdoc.html/factorial_peano_iso.html).
@end{itemize}

").