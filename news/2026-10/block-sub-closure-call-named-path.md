# A closure's call of a block-declared routine takes the named-call path again

ADR-12529 phase 2 made a closure call a routine declared in its block through
an upvalue holding the declaring activation's code object, so the closure
keeps calling its own activation's routine after the block exits. That call
went through the general closure entry instead of the named-call path, and
the inline `.map` path, which runs a recompiled body without an upvalue
array, resolved `&h` by name on every call (#13049).

- A `CallOnCodeVar` whose target is such a code object now runs its compiled
  routine on the named-call path (`call_compiled_function_named_in`), with
  the frame linked capture-first to the activation the code object captured,
  as the closure entry linked it.
- The recompiled `.map` body's `&h(...)` calls point at the closure's own
  upvalue for `&h` (`bind_origin_code_var_upvalues`), and the inline map loop
  installs the closure's upvalue array while it runs.
- `resolve_token_defs` and `has_proto_token` answer at once in a program that
  declared no token, instead of walking the current package's MRO on every
  `&name` resolution of a plain sub.

Release build, JIT off, `scripts/bench-det.sh` (base = `52eb5a95`, before the
slice that made the call a code-object call):

| benchmark | base | before | after |
|---|---:|---:|---:|
| bench-blocksub (callback through the inline map path) | 420.8M | 569.1M | 468.7M |
| bench-blocksub2 (a closure called directly) | 480.1M | 524.1M | 464.9M |

The direct call is now cheaper than before the slice. The map row is still
11% above base: what remains is the per-activation binding itself, the
`&h` resolution and capture that `bind_lexsub_upvalues` does each time the
callback closure is created.
