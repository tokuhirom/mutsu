# Zero-parameter closures run on the light closure path

The light closure call (#8335) serves a pointy block or placeholder block
whose body can observe nothing of a call frame, binding its arguments
straight into local slots. Its signature plan, `light_def_params`, treated a
signature with no parameter definitions as "no plan" and declined, so the
commonest callback of all, a zero-parameter `-> { ... }` called through a
code variable or a `&code` parameter, always took the full closure entry:
an environment clone, a caller frame and a writeback scan per call.

A closure with no parameters at all now gets an empty plan, served at exact
arity 0. A call that passes arguments still declines on the arity test, and
the general binder reports it with the usual "Too many positionals passed".

A zero-argument call of `my &pt = -> { 1 }` went from 2.1 us to 1.2 us, and
`code()` on a `&code` parameter from 3.7 us to 2.5 us (release build, rakudo
about 0.25 us; the rest of the gap is the call-floor work of #15737).
