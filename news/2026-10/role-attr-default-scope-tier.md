# A role attribute default no longer copies the role's scope into the env per construction

A role attribute default that is not a literal (`has Tag $.tag = Tag`) runs
when a composing class is constructed. It must still see the names that were
visible where the role was declared. Until now every construction inserted
each name of that captured scope that the running scope lacked into the live
env, evaluated the default, and removed them again: O(captured scope) env
writes per default per `.new` (#13018).

The captured scope is now held once per role as a `CaptureView`, the same
lookup-tier type a closure capture uses. A default runs over a block tier
whose capture fallback is that view, so a name the running scope lacks
resolves through it, below everything already visible. That is the precedence
the per-key insertion built by hand. What the default writes by name is merged
back from the tier afterwards. A write to a role-only name stays scoped to the
evaluation, as before.

`role R { has Int $.t = Int; has $.v }; class C does R {}; C.new(:v($_))`
went from ~876k to ~267k instructions per construction (debug build).
