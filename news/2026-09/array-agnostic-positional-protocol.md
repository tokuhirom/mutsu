# Array::Agnostic positional protocols work under mutsu

The real `Array::Agnostic` distribution now passes all 27 of its upstream
tests under mutsu. Its custom `Positional` implementations can use sparse
assignment, preserve holes across `shift`/`unshift`, and provide `BIND-POS`
and `CLEAR` protocol methods with the same dispatch and diagnostic behavior as
Rakudo.

The fixes cover generic positional protocol dispatch, lvalue method
assignment, initialized-hole metadata, and dynamic exception messages. The
regression pin is `t/collections/array-agnostic.t`.
