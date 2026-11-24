# 6.e `CALLERS::` searches every caller for a plain name

Under 6.e, `CALLERS::<$x>` of a caller's non-dynamic `my $x` gives a Failure
of X::Symbol::NotDynamic. mutsu looked only at the immediate caller frame for
a plain (non-twigil) name. Inside an `if`, `do` or `try` block it read only
the block's lexical outers, so it quietly answered Nil. A plain name two
callers up was read through the wrong frame's environment, so it answered
`(Any)`.

rakudo's `CALLERS::` searches every caller from the first outward, and the
nearest frame that has the name answers. mutsu now does the same under 6.e
(#18836):

- the run-time search reads each caller frame's own bindings;
- a block inside a routine falls back to the routine's callers for a name no
  enclosing scope declares.

6.d keeps its single-frame lookup.
