# Exception.die / Failure.die, and die $failure's message

`Exception` (and its `X::*`/`CX::*`/user subclasses) and `Failure` had no
`.die` method at all: `X::AdHoc.new(...).die` raised "No such method 'die'".
A handled `Failure`'s `.die` had the same gap -- the generic "unhandled
Failure explosion" mechanism happened to cover the unhandled case by
accident, but once a Failure was marked handled (e.g. via `.defined`),
`.die` had nothing left to catch it.

`.die` now throws exactly like `.throw`: immediately, carrying the
exception (or, for a `Failure`, the exception it wraps) as `$!`, regardless
of the Failure's handled state. This also fixes the `orelse .die` idiom
from `Language/perl-nutshell.rakudoc`.

Separately, `die $failure` (the sub form, and `X::AdHoc`-wrapping in
general) previously computed its message by calling `.Str` on the die
value, which for a `Failure` has no `message` attribute of its own and fell
back to the type repr, printing the literal `Failure()` instead of the
wrapped exception's message. It now re-throws the wrapped exception
directly, the same way the method form does.

Finally, a handled `Failure`'s `.gist` now shows a `(HANDLED)` prefix, both
when called directly and via the raw stringification fallback used inside
a container (e.g. `[$failure].gist`), which previously also rendered the
type repr `Failure()` for an embedded Failure.
