\title Using Ciao Prolog's Fair Search Rule

\begin{alert}
This is an example of slides with embedded, runnable code. 

- Hit the rectangular slides icon in top right to switch to slides mode. 

- Hit the arrows button to enter presentation mode.

- Hit the pencil button to edit the slides.

- Hit \key{ESC} to exit slides mode.

- Use \key{Share!} to share the slides.
\end{alert}

\begin{note}

These are a few slides adapted from the presentation for the paper 
[Teaching Pure LP with Prolog and a Fair Search Rule](https://ceur-ws.org/Vol-3799/paper2PEG2.0.pdf), 
in the [Prolog Education Group's (PEG 2.0) 2024 Workshop](https://prolog-lang.org/Education/PrologEducationWS2024.html)
at [ICLP 2024](https://www.iclp24.utdallas.edu/).

\end{note}


# A tradtional example - family relations.

```ciao_runnable
:- module(_,_).

%! \begin{focus}
mother_of(susan, mary).
mother_of(susan, john).
mother_of(mary, michael).

father_of(john, david).

grandmother_of(X,Y) :-
    mother_of(X,Z), mother_of(Z,Y).
grandmother_of(X,Y) :-
    mother_of(X,Z), father_of(Z,Y).
%! \end{focus}
```

We can load (clicking on \key{?}) and get answers to questions in different modes:

```ciao_runnable
?- mother_of(susan,Y).
```
```ciao_runnable
?- mother_of(X,mary).
```
```ciao_runnable
?- grandmother_of(X,Y).
```

**All these queries will return the correct solution and terminate using standard Prolog.**

All is good! However, things can quickly get more complicated...

# Non-termination rears its ugly head

```ciao_runnable
:- module(_,_).

%! \begin{focus}
mother_of(susan, mary).
mother_of(susan, john).
mother_of(mary, michael).

father_of(john, david).

parent(X,Y) :- mother_of(X,Y).
parent(X,Y) :- father_of(X,Y).

ancestor(X,Y) :- ancestor(X,Z), parent(Z,Y).
ancestor(X,Y) :- parent(X,Y).
%! \end{focus}
```

Now, for _any_ query, standard Prolog hangs:

```ciao_runnable
?- ancestor(X,Y).
```

# Non-termination rears its ugly head

```ciao_runnable
:- module(_,_).

%! \begin{focus}
mother_of(susan, mary).
mother_of(susan, john).
mother_of(mary, michael).

father_of(john, david).

parent(X,Y) :- mother_of(X,Y).
parent(X,Y) :- father_of(X,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- ancestor(X,Z), parent(Z,Y).
%! \end{focus}
```

**Changing the order of the ancestor/2 clauses**:

```ciao_runnable
?- ancestor(X,Y).
```

Now standard Prolog obtains all the answers... but then hangs.

# More complex cases

```ciao_runnable
:- module(_,_).

%! \begin{focus}
natural(0).
natural(s(X)) :- natural(X).

add(0,Y,Y) :- natural(Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(0,Y,0) :- natural(Y).
mult(s(X),Y,Z) :- add(W,Y,Z), mult(X,Y,W).

nat_square(X,Y) :- natural(X), natural(Y), mult(X,X,Y).
%! \end{focus}
```

Standard Prolog execution provides useful answers for, e.g.,
mode `nat_square(+,-)`.

```ciao_runnable
?- nat_square(s(s(0)), X).
```

and even for mode `nat_square(-,+)` which is really nice: 

```ciao_runnable
?- nat_square(X,s(s(s(s(0))))).
```

- The next obvious temptation is to try:

```ciao_runnable
?- nat_square(X,Y).
```

- but only the first, trivial answer is generated and then execution hangs.

(Btw, it cannot be avoided with, e.g., tabling.)

# Fairness to the (partial) rescue

- To deal with these more complex cases we have found it useful to
  provide a means for selectively switching to other (fair) search
  rules such as breadth-first or iterative deepening: 
  
```ciao
:- use_package(sr/bfall).
```

- Comes with Ciao Prolog, but can be implemented in any Prolog via
  meta-programming. 
  
- Of course, with a fair rule **everything "works well"** for **all
  possible queries**.
  
  - In the sense that all valid solutions will be found, even if
    possibly inefficiently.

# Fairness to the (partial) rescue

For example, our problematic query will now enumerate the infinite set
of correct answers.

```ciao_runnable
:- module(_,_).

%! \begin{focus}
natural(0).
natural(s(X)) :- natural(X).

add(0,Y,Y) :- natural(Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(0,Y,0) :- natural(Y).
mult(s(X),Y,Z) :- add(W,Y,Z), mult(X,Y,W).

nat_square(X,Y) :- natural(X), natural(Y), mult(X,X,Y).
%! \end{focus}
```

```ciao_runnable
?- nat_square(X,Y).
```

# Other benefits of the fair search rules

- Even for the cases in which depth-first works, in bf:

  - The ``quality'' in the solution enumeration greatly improves!

- For example, for the Peano example, in depth-first search:

```ciao_runnable
:- module(_,_).

%! \begin{focus}


natural(0).
natural(s(X)) :- natural(X).

add(0,Y,Y) :- natural(Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(0,Y,0) :- natural(Y).
mult(s(X),Y,Z) :- add(W,Y,Z), mult(X,Y,W).
%! \end{focus}
```

```ciao_runnable
?- mult(X,Y,Z).
```

- It is clear that some answers will never be generated. 
- Would be a bad trainer for a learning system!

# Other benefits of the fair search rules

- Even for the cases in which depth-first works

  - the ``quality'' in the solution enumeration greatly improves!

- While in breadth-first search:

```ciao_runnable
:- module(_,_).

%! \begin{focus}
:- use_package(sr/bfall).

natural(0).
natural(s(X)) :- natural(X).

add(0,Y,Y) :- natural(Y).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

mult(0,Y,0) :- natural(Y).
mult(s(X),Y,Z) :- add(W,Y,Z), mult(X,Y,W).
%! \end{focus}
```

```ciao_runnable
?- mult(X,Y,Z).
```
  
- Clearly more informative, satisfying, and useful for subsequent
  predicates.

@comment{
# 

@begin{alert}

In summary, we argue that the use of a fair search rule helps students
visualize the true potential of the (C)LP paradigm and Prolog -- that
it is possible indeed to solve problems by simply thinking logically
and/or inductively.

@end{alert}
}

# Other benefits of the fair search rules

- In Ciao Prolog marking predicates as types (or in general as
  properties) allows them to be used in assertions. 
  
- However, they remain regular predicates and can be:

  - called as any other, 
  - used as run-time tests (dynamic checking), 
  - "run backwards" to generate examples or test cases
    (property-based testing), etc.

# Other benefits of the fair search rules

- In Ciao Prolog marking predicates as types (or in general as
  properties) allows them to be used in assertions. 
  
- However, remain regular predicates can be:

  - called as any other, 
  - used as run-time tests (dynamic checking), 
  - **"run backwards" to generate examples or test cases**
    (property-based testing), etc.

For example: 

```ciao_runnable
:- module(_,_).

%! \begin{focus}
:- use_package([assertions,regtypes]).
% :- use_package(sr/bfall).
  
:- regtype color/1.
color(red).
color(green).
color(blue).

:- regtype colorlist/1.
colorlist([]).
colorlist([H|T]) :- color(H), colorlist(T).

%! \end{focus}
```

```ciao_runnable
?- colorlist(L).
```

- Depth-first produces correct answers, but not very satisfying.
- Breadth-first much better!


# Other benefits of the fair search rules

- In Ciao Prolog marking predicates as types (or in general as
  properties) allows them to be used in assertions. 
  
- However, remain regular predicates can be:

  - called as any other, 
  - used as run-time tests (dynamic checking), 
  - **"run backwards" to generate examples or test cases**
    (property-based testing), etc.

For example (in Ciao's *functional notation*): 

```ciao_runnable
:- module(_,_).

%! \begin{focus}
:- use_package([fsyntax,assertions,regtypes]).
% :- use_package(sr/bfall).
  
:- regtype color/1. 
color := red | green | blue.



:- regtype colorlist/1. 
colorlist := [] | [~color|~colorlist].

%! \end{focus}
```

```ciao_runnable
?- colorlist(L).
```

- Depth-first produces correct answers, but not very satisfying.
- Breadth-first much better!


