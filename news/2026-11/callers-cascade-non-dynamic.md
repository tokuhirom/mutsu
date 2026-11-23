# 6.e `CALLERS::` searches every caller for a plain name

Under 6.e, `CALLERS::<$x>` of a caller's non-dynamic `my $x` gives a Failure
of X::Symbol::NotDynamic. mutsu looked only at the immediate caller frame for
a plain (non-twigil) name, so from inside a `try` thunk, a `do` block, an
`if` block or a called block (each runs as its own call) it read the routine's
own frame and quietly answered Nil. rakudo's `CALLERS::` searches every caller
from the first outward and the nearest frame that has the name answers; mutsu
now does the same under 6.e (#18836). 6.d keeps its single-frame lookup.
