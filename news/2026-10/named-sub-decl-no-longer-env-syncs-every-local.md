# Declaring a named sub no longer env-syncs every local of the frame

A frame that declared any named sub used to mark **every** one of its locals
`needs_env_sync`, because the sub's body is registered lazily through
`RegisterDecl` and the frame could not see which lexicals it reads by name.
Every store in a script's top-level loop therefore paid an env mirror write
(`set_env_plain_lexical` → `set_shared_var_sym` → `Env::insert_sym`, ~470 Ir)
as soon as the file declared a sub — about 15% of the per-iteration cost of
the `f()` call benchmark (#10960).

The body is compiled at its declaration, so its by-name reads are known there.
The compiler (`src/compiler/lazy_body_env_sync.rs`) now resolves them — the
body's free reads and writes (nested closures, nested subs, parameter
defaults and `gather`/`whenever` bodies included), rw-arg-sink targets, and
at any closure depth the scalars mutated in place and bare callee names — to
the declaring frame's slots, and marks the plan bounded. `compute_needs_env_sync`
folds only those slots. A body that resolves names no op scan can bound (an
interpolating or indirect regex, a dynamic substitution replacement, a
deferred phaser, or a nested class/role declaration) keeps the old
every-local fold, as do class and role declarations themselves.

On the issue's repro (100,000 iterations of `$t = f(); $i = $i + 1`,
callgrind, profiling build) `exec_set_local_op`'s inclusive Ir dropped from
166,305,301 to 71,904,726 — identical to the same loop with `sub f` removed
— and the whole program from 920.6M to 826.2M Ir.
