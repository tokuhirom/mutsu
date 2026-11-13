# A CHECK-rewritten unit's restart keeps the first compile's compile-time state

A CHECK phaser may rewrite `$*CU`; the unit then continues from the rewritten tree (#17501). That
restart used to run the unit's BEGIN prologue and CHECK phasers a second time, over the tree they
had just rewritten. `BEGIN say "begin"` printed twice (#17676), and Red's t/106 — which runs
`red-compile` in a CHECK phaser — saw its own rewritten callbacks on the second run and died with
"Regex callback was not compiled" (#17681).

The restart now keeps the first compile's compile-time state instead of re-running it, as rakudo
does (ADR-17681):

- a statement-form BEGIN or CHECK phaser body is skipped on the restart (it already ran), while a
  value-form BEGIN and a constant initializer, whose value is needed, still run;
- a top-level `my` declaration without an initializer adopts the first compile's binding of the same
  name — keeping whatever its BEGIN/CHECK phasers wrote into it — instead of resetting it, while a
  declaration with an initializer still runs its initializer;
- the first compile's top-level locals are published into the frame's environment before the
  restart, so the adopted scalar lexicals (which live in local slots the restart's separate code
  object cannot reach by name) resolve.

Red's t/106 now matches `raku` exactly, and the earlier attempt's regression in
`t/rakuast/rakuast-resolver-scope-stack.t` (whose CHECK phaser pushes into a top-level `my @seen`)
stays fixed.
