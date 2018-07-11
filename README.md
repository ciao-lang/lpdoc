# Documentation Generator

`lpdoc` is an *automatic program documentation generator* for (C)LP
systems.

`lpdoc` generates documentation from one or more source files for a
logic program (including ISO-Prolog, Ciao, and other CLP). It is
particularly useful for documenting library modules, for which it
automatically generates a description of the module interface. `lpdoc`
can also be used quite successfully to document full applications and
to format single documentation files.

The quality of the documentation generated can be greatly enhanced by
including within the program text:

 - *assertions* (types, modes, etc. ...) for the predicates in
   the program, and 
 - *machine-readable comments* (in the ``literate programming''
   style).

The assertions and comments included in the source file need to be
written using the Ciao system *assertion language*.

`lpdoc` can generate documentation in multiple formats, including
@tt{HTML}, @tt{pdf}, @tt{info}, and @tt{ascii}.

`lpdoc` is distributed under the GNU general public license.
