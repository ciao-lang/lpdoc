:- use_package([assertions]).

% :- doc(filetype, documentation).
:- doc(filetype, part).

:- doc(title,"Including Editable and Runnable Examples").

:- doc(author,"The Ciao Development Team").

:- doc(module,"

Active Logic Documents (@apl{ALD}s) are documents with @em{embedded} editable and runnable examples that can be
generated easily using the @apl{LPdoc} tool. Code fragments can be
automatically rendered as editable panes that can be run in place
(embedded playground) or loaded into a separate playground. The
following example: 

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

Can be generated including in the source file (e.g., in markdown) the
following code:

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

There is essentially one Ciao Prolog top level per page; all programs
in @apl{ALD} are loaded into this Ciao Prolog top level and all queries
in the page are executed in that top level, against all the code
(possibly separate modules) that has been loaded into the top level at
that point.

@begin{note}
The list of the available [types of runnable](/ciao/build/doc/lpdoc.html/ALD_runnables.html).
@end{note}

@section{Full page example}

The following pointers provide a complete example of a class exercise
that uses embedded code, showing the full source and the full output:

@begin{itemize}
@item This is the [source of the page (written in this case in markdown)](/ciao/build/doc/lpdoc.html/factorial_peano_iso_source.html)
@item and this is the [ALD produced by LPdoc](/ciao/build/doc/lpdoc.html/factorial_peano_iso.html).
@end{itemize}

").