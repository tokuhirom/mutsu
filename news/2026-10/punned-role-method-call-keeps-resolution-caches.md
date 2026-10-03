# A method call on a role type object no longer flushes the resolution caches

Calling a method on a role type object (`R.m`, a pun) ran
`run_pun_role_bodies` first, which checks a once-per-role memo before
running the role body. It probed that memo through `registry_mut()` (and
cloned the whole `RoleDef` before even looking), and every `registry_mut()`
bumps the registry write generation — the key every generation-scoped
resolution cache (`user_method_probe_memo` and friends) is flushed on. So
every punned call threw those caches away and redid their walks, among them
`grammar_has_user_method_sym`'s `class_is_grammar` check, whose
`resolved_class_parents` scans the whole class table for a name that is not
a class: ~170k instructions per call (#11115).

The memo is now probed under the read lock first; the write lock (and the
`RoleDef` clone) is taken only the one time the body actually runs.

On a 100,000-iteration loop of `$t = R.m` after `role R { method m() { 1 } }`
(profiling build, callgrind) the run drops from 20.66G to 2.35G instructions,
against 1.63G for the same loop with `class R`; `resolved_class_parents` is
called twice instead of 100,000 times. Wall clock goes from 1.63 s to 0.30 s
(0.26 s for the class).
