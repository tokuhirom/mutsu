# A package variable written in a closure is not frozen into the closure

`my &g = do { sub g { my $r = $P::tz; $P::tz = 'd'; $r } }` kept its own copy
of `$P::tz` after the first call: a later `$P::tz = 'X'` from the mainline was
invisible to the next `g()`, and `g`'s second write never reached anyone else.

The closure-call exit persists each free variable the body writes as
per-instance captured state, so that two closures from one factory keep
separate counters. A package-qualified variable went through the same loop,
but it is a global, not a lexical capture. The persistence store now refuses
package variables (`qualified::is_package_var`: a `::`-qualified key whose
head is a real package, not a pseudo-stash like `OUTER::` or an internal
`__mutsu_outer::` key), so every call reads and writes the one package
variable.

This made UserTimezone's override test pass: its `user-timezone` sub, declared
inside `sub EXPORT`, caches into `$UserTimezone::timezone`, and the exported
`override-user-timezone` writes the same variable. All three of its test files
now pass under mutsu.
