:- use_package([assertions]).

% :- doc(filetype, documentation).
:- doc(filetype, part).

:- doc(title,"Summary: Types of Runnable").

:- doc(module,"

The combination of @tt{```ciao_runnable} and @tt{```} results in an interactive panel in the output. This panel displays editable and executable code. The type of executability varies depending on the directives employed. Below, we provide an overview of the available types along with a brief explanation for each:

@begin{itemize}

@item Code @bf{focus}:
By utilizing the begin focus and end focus directives:

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

you can selectively display certain sections of the code in the output. This feature allows you to hide irrelevant lines of code, such as module declarations, imports, or auxiliary code, that may not contribute to the current discussion.
To reload or reset the program's state, simply click on the green tick mark located in the top left corner of the pane. This functionality enables you to reload the code at any time.

@item @bf{Queries}: You have the option to include one or multiple queries.

~~~
```ciao_runnable
?- qsort([1,5,4,2,3],X).
```
~~~

@item Interactive @bf{exercises}: In this interactive pane, you can engage in exercises by editing the code and adding comments, which serve as hints or instructions. Alternatively, the description of the exercise may be provided in the surrounding text. By clicking on the yellow face icon, the code undergoes evaluation, including the execution of hidden unit tests, which provide feedback to the student.
If a student wishes to give up and view the solution, they can request it, and the proposed solution will be displayed.

Segments enclosed within hint directives function similarly to focus segments, offering hints or instructions. However, if the solution is requested, the hint segment will be replaced with the corresponding solution, indicated by the solution directives.

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

@item @bf{miniplayground}: A miniplayground can be displayed, offering a simplified version of the playground. It retains the structure of the playground but includes only the debugger feature, omitting other functionalities.

~~~
```ciao_runnable
:- module(_, _, [assertions,sr/bfall]).
%! \\begin{miniplayground}
:- module(_, [qsort/2], []).

qsort(Data, Out) :-
    qsort_(Data, Out, []).

qsort_([], R, R).
qsort_([X|L], R, R0) :-
    partition(L, X, L1, L2),
    qsort_(L2, R1, R0),
    qsort_(L1, R, [X|R1]).

partition([],_,[],[]).
partition([X|L],Y,[X|L1],L2) :-
    X =< Y, !,
    partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
    partition(L,Y,L1,L2).
%! \\end{miniplayground}
```
~~~

Result:

```ciao_runnable
:- module(_, _, [assertions,sr/bfall]).
%! \\begin{miniplayground}
:- module(_, [qsort/2], []).

qsort(Data, Out) :-
    qsort_(Data, Out, []).

qsort_([], R, R).
qsort_([X|L], R, R0) :-
    partition(L, X, L1, L2),
    qsort_(L2, R1, R0),
    qsort_(L1, R, [X|R1]).

partition([],_,[],[]).
partition([X|L],Y,[X|L1],L2) :-
    X =< Y, !,
    partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
    partition(L,Y,L1,L2).
%! \\end{miniplayground}
```
@end{itemize}

The `ciao_runnable` language tag also allows the following additional annotations to interact with the browser using Javascript and/or Prolog code running through WebAssembly. 

@begin{alert}
These are experimental features whose API may change in future versions. Usage requires knowledge of HTML, Javascript (and some internals of `ciao_playground.js`).
@end{alert}

@begin{itemize}

@item @bf{jseval}: Escape mechanism to allow execution of arbitrary Javascript code. The text within `jseval` is executed at document initialization (following the sequential order of playground cells) as a Javascript asynchronous function. Typically this is done as:

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
Allow dynamically generated HTML fragments. See for example `catalog_ui/bundles_dyn.pl` in the website bundle. It requires a JSON structure that is passed to `setup_dynpreview` (see `ciao_playground.js`). For example:

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
The `render_pred` field specifies the predicate that will generate the HTML contents (as a string, e.g., using @lib{pillow}). The `state_hash` field specifies if the URL hash is passed as parameter (e.g., to maintain a state). The rest of fields indicate the bundle dependencies and initialization goals.

@end{itemize}

").