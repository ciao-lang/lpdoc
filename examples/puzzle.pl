:- module(puzzle,[solution/2]).
:- use_package(doccomments).

%! \title Example - A simple puzzle

%! \module Write a Prolog program that generates (as a list) a sequence that: 
% 
% - includes three 1's, three 2's, . . . , and three 9's, 
% 
% - and between each digit i and the next i there are exactly i
%   elements.
%   
% E.g.:
% 
% `[1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7]`

%! solution(S): `S` is a solution of the puzzle

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

%! sublist( Y, XYZ ): list `Y` is a sublist of list `XYZ`.

sublist( Y, XYZ ) :- 
 append( _X, YZ, XYZ ),  append( Y, _Z, YZ ).

%! append( K, L, M ):  `M` is the concatenation of lists `K` and  `L`.
% (It is a built-in in most Prologs.)

% append( [], X, X ).
% append( [E|X], Y, [E|Z] )  :-  append( X, Y, Z ).
