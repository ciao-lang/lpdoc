
%% Syntax files defining assertions, modes, types, etc.
:- use_package([assertions,basicmodes,nativeprops,fsyntax,regtypes,dynamic]).

%% We declare the type of file (needed for 'user'-type files)
%% :- doc(filetype,user).

%% "doc" declarations provide additional information
:- doc(title,"An example of a simple file").  
:- doc(author,"Bob Kowalski").  
:- doc(module,"A general comment on the file.").

%% Some op definitions (which are public).
:- op(975, xfx,(fooop)).
:- op(978, xfx,(barop)).

%% Declaring a new declaration
:- new_declaration(pred/1,on).

%% Some standard declarations which may be documented
:- data p/1.
:- multifile p/3.
:- dynamic p/3.

%% This is a type definition in Prolog syntax: declaration and code
:- regtype bar(X) # "@var{X} is an acceptable kind of bar.".

bar(night).
bar(day).

%% A type definition in 'typedef' syntax, using 
%% (will be expanded to code as above)
%% :- typedef aorb ::= ^a;^b.
%% :- typedef listof_or_aorb(X) ::= list(X);aorb.

:- regtype aorb/1. 

aorb := a. 
aorb := b. 

:- regtype listof_or_aorb/1. 

listof_or_aorb(X) := ~list(X).
listof_or_aorb(_) := ~aorb.

%% This is a property definition:
:- prop long(L) # "@var{L} is rather long.".

long(L) :- 
    length(L,N),
    N>100.

%% Now, a series of assertions:
:- entry p/3 
    : ground * var * var 
    # "This declares the entry mode of this exported predicate (i.e., 
      how it is called @em{from outside}).".

:- doc(p/3,"A @bf{general comment} on the predicate." ).
%% Documenting some typical usages of the predicate
:- pred p/3 
    :  integer * integer * var 
    => integer * integer * list 
    +  (iso,not_fails) 
    #  "This mode is nice.".
:- pred p(Preds,Value,Assoc) 
    :  var     * var     * list
    => integer * integer * list 
    +  not_fails # "This mode is also nice.".
:- pred p/3 
    => list * integer * list 
    +  (not_fails,not_fails) # "Just playing around.".
:- calls p/3 
    :  int * flt * var 
    #  "This documents the calls only".

:- pred q(A) 
    :  list(A) 
    => (list(A),ground(A)) 
    +  not_fails
    # "Foo".
:- pred q(A) 
    # "Not a bad use at all.".

:- pred q/2 
    :  var * {ground,integer} 
    => {ground,integer} * integer.
:- pred q/2 
    :: integer * list
    #  "Non-moded types are best used this way.".

:- pred p/1 : var => list.
 
:- pred r(A) 
    :  list(A) 
    => (list(integer,A),ground(A)) 
    +  not_fails 
    #  "This uses parametric types".

:- doc(doinclude,s/1). %% Forces documentation even if not exported
:- pred s(A) 
    :  list(A) 
    => (list(A),ground(A)) 
    +  not_fails.

:- modedef og(A) 
    => ground(A) 
    #  "This is a @em{mode} definition: the output is ground.".

:- doc(doinclude,og/2).
:- modedef og(A,T) 
    :: regtype(A,T) 
    => ground(A) 
    # "This is a @em{parametric mode definition}.".

:- pred t(+A,-B,?C,@D,og(E)) 
    :: list * list * integer * integer * list 
    :  long(B)
    => (ground(C),ground(A)) 
    +  not_fails 
    #  "This predicate uses @em{modes} extensively.".

%% Some other miscellaneous assertions:

%% Check is default assertion status anyway...
:- check pred u(+,-,og).
:- check pred u(integer,list(mytype),integer).

%% ``true'' status is normally compiler output
:- true pred w(+list(mytype)).

:- doc(doinclude,p/5).
:- trust pred p(og(integer),in,@list(integer),-,+A) + cost(1+length(A)).
