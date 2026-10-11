# Six more of Rakudo's `p6*` nqp ops: the ones that read the running frame

Rakudo has nine `p6*` ops that read state a MoarVM frame keeps and mutsu does not: the
call's arguments, its signature binding, the first run of its `state` variables, the `FIRST`
and `PRE` flags, and the static outer. ADR-11505 decides each one from what `raku` actually
answers, and implements six of them (#11505). That takes the category from 17 / 26 to 23 / 26
in `docs/nqp-op-coverage.md`.

- `nqp::p6argvmarray()` answers the call's positional arguments as a raw list, with a
  method's invocant first. It reads the capture `nqp::usecapture` already records. Only code
  that mentions one of these ops records a capture.
- `nqp::p6bindsig()` and `nqp::p6trybindsig()` take Rakudo's already-bound branch, because
  mutsu binds every frame before its body runs. They answer null and 1, and nothing is
  rebound.
- `nqp::p6setpre()` and `nqp::p6clearpre()` answer null. The flag they set has no reader a
  Raku program can reach.
- `nqp::p6stateinit()` answers 1 on the first run of a closure clone whose frame declares
  `state` variables. A parse-time pass lowers it onto a hidden `state` variable in that
  frame, so it agrees with mutsu's own `state` initialisation. The frame follows Rakudo's
  inlining: a statement-level `if` or `for` body belongs to the enclosing routine, and a
  `map` block is a frame of its own.

The work found a bug. A one-expression routine such as `sub f($a, $b) { nqp::usecapture() }`
ran on the TRIR fast path, which records no capture, so it answered an empty one. TRIR now
declines routines that read their capture.

`p6setfirstflag`, `p6takefirstflag` and `p6staticouter` are decided in the ADR and come next.
