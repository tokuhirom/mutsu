# `DYNAMIC::<$*x> := v` binds the frame that declares `my $*x`

In 6.e, binding through the `DYNAMIC::` pseudo-stash from a callee now
rebinds the nearest frame's `my $*x`. Before, it always bound the `GLOBAL` or
`PROCESS` fallback, so a caller reading its own `$*x` afterwards still saw the
old value. This also covers a promise's `.then` block running on another
thread: after `await`, the awaiting frame sees the new value (#18855, roast
`S02-names/pseudo-6e.t` test 201).
