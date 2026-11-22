# `Failure.raku` renders as Rakudo does

`Failure.raku` is Rakudo's `self.Mu::raku`, shown as
`Failure.new(exception => X::AdHoc.new(payload => "oops"), backtrace => Backtrace.new)`.
A handled Failure is wrapped as `&CORE::infix:<orelse>(..., *.self)`, which
rebuilds it defused. mutsu used to render its own expressions,
`Failure.new("oops")` and `do { my $f = Failure.new("oops"); $f.Bool; $f }`,
which also showed up inside a container's `.raku` (`Set.new($failure).raku`)
(#18603).

The row renders an `X::AdHoc` without an interpreter. Any other exception is
rendered by the instance's own `.raku`.

For that form to round-trip (`$failure.raku.EVAL.handled`), the routine form of
`orelse`, `andthen` and `notandthen` now folds its operands as Rakudo's
`infix:<orelse>(+a)` does. A `Callable` operand is called with the value so far
(`&infix:<orelse>(Nil, *.self)` is `Nil`, not the `WhateverCode`), and the
definedness test handles a Failure, as `.defined` does.
