# END phasers install at COMPILE time in rakudo, including ones never reached

`todo/tickets/end-phaser-run-order-is-not-reverse-installation.md` asked for the
full ordering contract to be measured against `raku` before anyone changed the
comparator. It was, on 2026-09-01, and the measurement turned up a second,
larger divergence than the ordering the ticket reported. The ordering half is
fixed (`news/2026-08/end-phaser-source-order.md`); this file records what is
left.

## The measured contract

Rakudo installs **every** `END` in a compunit when that compunit is *compiled*,
in source order, and runs them in reverse. A module's compunit is compiled at
its `use`, which is why a module's ENDs run after the script's. An `EVAL`'d
snippet is compiled at run time, so its ENDs install last and run first.

The consequence mutsu does not implement:

```raku
if False { END { say "never-run-block" } }
sub g      { END { say "uncalled-sub" } }
for 1..3   { END { say "loop" } }
END        { say "main" }
```

```
raku : main  loop  uncalled-sub  never-run-block
mutsu: loop  main
```

An END inside a block that never executes, or a sub that is never called, still
runs in rakudo — and an END inside a loop body runs exactly **once**, not once
per iteration (mutsu already gets the once part right, via
`register_end_phaser_site`).

## Why mutsu cannot do this today

mutsu registers an END when execution reaches its `PhaserEnd` opcode, and the
registration is also where the phaser's captured `Env` comes from
(`push_end_phaser_ordered` clones `self.env`; `update_end_phaser_envs` later
freezes it against the dying scope). An END that is never reached has no scope
to capture, so "install everything at compile time" cannot simply move the
existing call: it needs the *ordering slot* to be assigned at compile time while
the env capture stays where it is, with a defined answer for what an unreached
phaser's body sees. Rakudo's answer is the declaring scope's lexicals as they
would be, i.e. undefined for a block that never ran — worth measuring before
implementing.

## The smaller residual the ordering fix left

A main-compunit END is now ordered by its **source line**, which is exact for
every realistic program but ties when several ENDs share one physical line:

```raku
{ END { say 1 } }; { END { say 2 } }; END { say 3 }   # all on one line
raku : 3 2 1
mutsu: 2 1 3
```

Within a tie the hoisted mainline ENDs sort first (lowest registration
sequence), and no fixed tie-break can be right in both directions — `END {3}; {
END {1} }` on one line wants the opposite order from the case above. It needs a
real per-END source index, which is the same compile-time numbering the
unreached-END fix needs. Doing both at once is the coherent unit of work; a
field on `ast::Stmt::Phaser` is the obvious carrier, but it has ~92 construction
sites, so a pre-pass that numbers ENDs in source order and threads the index to
both registration sites is likely the cheaper shape.

## Pins that must keep passing

`t/end-phaser-source-order.t` (the reverse-source-order contract and the eager
hoist that keeps an END running when the mainline dies),
`t/end-phaser-module-order.t` (module load order, and the `EVAL`-loaded module
that must install last), and `roast/S04-phasers/*`.
