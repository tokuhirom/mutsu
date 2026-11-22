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
