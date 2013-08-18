# Pretty-printing Combinators for Racket

Pretty-printing combinators, loosely following [Azero and
Swierstra](http://www.cs.uu.nl/wiki/HUT/PrettyPrintingCombinators) but
using ordered choice instead of unordered choice. The change lets us
find an OK-ish solution in reasonable time, but of course means we're
not guaranteed to find an optimal solution.

**WARNING**: This code is not ready for use, as it is (very)
superlinear in some cases still, despite no longer relying on
examination of the entire tree.

The main interfaces are:

 - document construction via combinators

     - `(empty-doc)`
     - `(indent Natural Document)`
     - `(above Document Document)`
     - `(above* Document ...)`
     - `(beside Document Document)`, `(beside/space Document Document)`
     - `(beside* Document ...)`, `(beside*/space Document ...)`
     - `(choice Document-expr ...)`

 - `doc->string` lays out and renders a Document to a string

 - `sexp->doc` and `sexp->string` render S-expressions representing Racket code
