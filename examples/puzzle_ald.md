\title Example - A simple puzzle

Write a Prolog program that generates (as a list) a sequence that: 

- includes three 1's, three 2's, . . . , and three 9's, 

- and between each digit i and the next i there are exactly i
  elements.
  
E.g.:

`[1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7]`

```ciao_runnable
% Headers (hidden):
:- module(_,_).
:- use_package([assertions,nativeprops]).
:- use_module(library(unittest/unittest_props)).


% Testing code (hidden):
:- test solution(S) 
   => member(S,[[1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7],
                [1,8,1,9,1,5,2,6,7,2,8,5,2,9,6,4,7,5,3,8,4,6,3,9,7,4,3],
                [1,9,1,6,1,8,2,5,7,2,6,9,2,5,8,4,7,6,3,5,4,9,3,8,7,4,3],
                [3,4,7,8,3,9,4,5,3,6,7,4,8,5,2,9,6,2,7,5,2,8,1,6,1,9,1],
                [3,4,7,9,3,6,4,8,3,5,7,4,6,9,2,5,8,2,7,6,2,5,1,9,1,8,1],
                [7,5,3,8,6,9,3,5,7,4,3,6,8,5,4,9,7,2,6,4,2,8,1,2,1,9,1]])
   + (try_sols(8),num_solutions(6)).


% Hints/instructions (visible) and solution (initially hidden)

%! \begin{hint}
% Write the solution as the following predicate, such that S is a
% solution to the puzzle: 

solution(S) :- 
%! \end{hint}


%! \begin{solution}
solution( S ) :- 
 % S must be a 27 element list: 
 S = [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
 % (You can also do: N is 3*9, length(S,N))
 sublist( [1,_,1,_,1], S ),
 sublist( [2,_,_,2,_,_,2], S ),
 sublist( [3,_,_,_,3,_,_,_,3], S ),
 sublist( [4,_,_,_,_,4,_,_,_,_,4], S ),
 sublist( [5,_,_,_,_,_,5,_,_,_,_,_,5], S ),
 sublist( [6,_,_,_,_,_,_,6,_,_,_,_,_,_,6], S ),
 sublist( [7,_,_,_,_,_,_,_,7,_,_,_,_,_,_,_,7], S ),
 sublist( [8,_,_,_,_,_,_,_,_,8,_,_,_,_,_,_,_,_,8], S ),
 sublist( [9,_,_,_,_,_,_,_,_,_,9,_,_,_,_,_,_,_,_,_,9], S ).

% sublist(X,Y): `X` is a sublist of `Y`.

sublist( Y, XYZ ) :- 
   append( _X, YZ, XYZ ),
   append( Y, _Z, YZ ).
%! \end{solution}
```

- To test your solution, press the 'thinking face'. 

- To run your solution load it by pressing the question mark above,
  and use the following query: 

```ciao_runnable
?- solution(S).
```

- Alternatively, you can try and debug your solution in the
  playground by pressing the arrow.
