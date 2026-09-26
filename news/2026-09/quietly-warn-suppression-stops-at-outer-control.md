# `quietly` stops an outer `CONTROL` from ever seeing its warnings

`quietly { warn "..." }` only suppressed *printing* the warning; the underlying `CX::Warn` control
exception still traveled out to any enclosing `CONTROL { when CX::Warn { ... } }` block, which could
then act on it -- even though rakudo's `quietly` installs its own resume-everything `CONTROL` and
never lets the warning escape at all. The same primitive backs the Hash hyper's warning suppression
(`»op«` reading a missing key), so a user-defined infix used inside a hash hyper had the same leak.

The fix tracks, at each `push_warn_suppression` call, how many `CONTROL` handlers were registered so
far. A `warn` raised while suppressed now only offers itself to handlers registered *after* that
point -- i.e. declared lexically inside the suppressed region, which rakudo's nesting order still
lets see it first -- and resumes in place without reaching anything outside it. Pinned by new cases
in `t/control/quietly.t` and `t/lang/operators/hyper-hash-missing-key-any.t`.

While chasing this, a separate, pre-existing bug turned up: a `die` that escapes a `quietly { }`
block leaks its suppression depth for the rest of the process, silencing every later warning. That
is unrelated to this fix (confirmed present on `main` before it) and is tracked as
[#9656](https://github.com/tokuhirom/mutsu/issues/9656).
