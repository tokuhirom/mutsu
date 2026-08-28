# An END phaser's frozen capture is no longer clobbered by a same-named closure

An `END` phaser installed inside a scope that later dies (a bare block, a
`BEGIN`, a called routine's frame) freezes its captured lexicals: at the
moment the declaring scope exits, mutsu records which captured names have no
surviving live binding (`EndPhaser::dead_keys`) so the phaser's own snapshot
becomes authoritative for those names, exactly as Raku's real closure-over-a-
container semantics require.

That freeze was silently undone by an unrelated refresh. After every closure
call, `update_end_phaser_envs_for_keys()` walked **every** registered END
phaser and, for any name the *called closure itself* had captured, overwrote
the matching entry in each phaser's captured env with whatever value that
name currently held live — with no check that the phaser's binding and the
closure's binding were the same variable. Sharing a short name was enough:

```raku
sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;                 # a DIFFERENT binding that merely shares the name
callit { $a };             # a called closure that only READS it
```

mutsu printed `END1 (want 42): 0` — the closure's read of the *mainline's*
`$a` (0) clobbered the phaser's own frozen `$a` (42), which had already died
with its block. The write-side variant was worse: a runtime-resolved write
through the closure (`callit { $::('a') = 7 }`) leaked the written value (7)
into the phaser's captured copy too, even though the write landed on the
unrelated mainline binding.

The fix makes the post-call refresh respect the same freeze the block-exit
path already establishes: `update_end_phaser_envs_for_keys()` now skips any
key already present in a phaser's own `dead_keys` set. A frozen name is that
phaser's last surviving binding and must never be overwritten by an
unrelated same-named capture found elsewhere; every other captured name is
still a live variable and keeps propagating its later mutations exactly as
before (the original 2026-05 fix this refresh exists for — a closure that
mutates the *same* still-live binding an END phaser captured, whether called
directly or through another sub).

Fixes `roast/S04-phasers/end.t` tests 6 and 7 under `MUTSU_REAL_TEST=1` (the
vendored upstream `Test.rakumod`, whose real `lives-ok` calls the block it is
given — exactly the "called closure" ingredient the bug needed). The file was
already on `roast-whitelist.txt` and continues to pass under mutsu's native
`Test` provider, which never called the block the same way and so never
exercised the bug.

Pin: `t/end-phaser-same-name-different-binding.t` (7 assertions: the read-side
and write-side repros, two negative controls with no name collision at all,
a two-dead-scope-phasers case, and two positive controls confirming the
original same-binding propagation still works) — green under real `raku`.

A related, independent bug was noted while investigating this one and left
open: `todo/tickets/end-phaser-run-order-is-not-reverse-installation.md`
(mutsu does not run `END` phasers in strict reverse-installation order). It
does not share this fix's root cause and is unaffected by it.
