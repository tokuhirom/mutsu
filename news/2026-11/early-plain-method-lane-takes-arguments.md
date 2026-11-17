# The early plain-method lane serves calls with arguments

The plain-method lane (#8880) is a memo in front of `CallMethodMut`'s probe
chain. It records each `(class, method, argument type keys)` triple whose
dispatch has been observed reaching the user-method tail. It has an early
form, which checks that memo before the opcode-level receiver probes. Until
now that early form served only calls without arguments, so `$o.m($x)` still
ran the whole stretch before the late gate found its memo entry.

The early form now takes arguments too.

- **Calls it refuses.** It does not take a call whose arguments the stretch
  would transform before keying them:
  - a `|` spread at the call site;
  - a `VarRef`;
  - a value that `hllize_for_binding` rewrites;
  - the `AT-KEY` family, whose object key is stringified through user code.

  For every call it does take, the key it computes from the arguments as
  they stand is the late gate's key.
- **Effects it reproduces.** The stretch has two non-probe effects on such
  arguments, and a hit reproduces both: it publishes the call's argument
  sources, and it reifies a deferred `.map`/`.grep` argument.

Callgrind slope (profiling build, 2,000 to 4,000 iterations, second run) for
`$k.set('x')` inside a `for` loop: 19,920 to 18,600 Ir per call (-6.6%). This
is a reduction of the general method-call floor (#15737). It is not a ratio
change for any ecosystem file on its own.
