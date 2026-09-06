# An unreached END phaser reads its own block's lexicals as Nil, not Any

`news/2026-09/end-phasers-install-at-compile-time.md` made mutsu install every
`END` a compunit declares at compile time, so one inside a block that never runs
— or a sub that is never called — now runs at exit, as it does in rakudo. One
property of what such a phaser's body *sees* is still off.

## Repro

```raku
if False { my $x = 1; END { say $x.^name; say $x.raku; say $x.defined } }
say "mainline";
```

```
raku : mainline / Any   / Any / False
mutsu: mainline / Nil   / Nil / False
```

Measured against rakudo 2026.07 and mutsu at the commit that landed the
compile-time installation. `.defined` already agrees (both `False`), and a
container-sigil lexical already agrees — `if False { my @a; END { @a.push(1);
say @a.elems } }` prints `1` in both — so this shows up only through `.^name` /
`.raku` / `.WHAT` on a `$`-sigil lexical.

## Root cause

`Interpreter::preinstall_end_phaser` (`src/runtime/accessors_misc.rs`) gives a
pre-installed phaser an **empty** captured `Env`, deliberately: an un-captured
phaser must not overlay a stale copy of anything, so every name its body
mentions resolves against the live exit-time env instead. A lexical of a block
that never ran is absent from that env, and mutsu's variable read for an absent
name yields `Nil`. Rakudo instead has a real lexical container there, declared
by the compiler and never assigned, which reads as `Any`.

## Why it was not fixed with the rest

The fix is for the pre-pass (`src/runtime/end_preregister.rs`) to seed the
phaser's env with every lexical name visible at the declaration point — the
`my`/`state` declarations of each enclosing block along the descent path, plus
the enclosing routine's parameters — bound to `Any`, and to mark them in
`EndPhaser::dead_keys` so they win over a live same-named variable further out
(rakudo's inner declaration shadows it too). The blast radius is genuinely small
(these entries only ever apply to a phaser that is never captured; a capture
replaces env and `dead_keys` wholesale), but "which names are lexically visible
here" is a scope analysis the AST has no single authority for in mutsu, and
roast has no coverage of an unreached END reading a lexical — so a wrong answer
would not be caught by CI. It wants to be done deliberately, with its own pins,
rather than folded into the installation change.

## Affected files

- `src/runtime/end_preregister.rs` — the walker that would collect the names.
- `src/runtime/accessors_misc.rs` — `preinstall_end_phaser`, which would take
  them and seed the env.
- `t/end-phaser-compile-time-install.t` — where the pin belongs (it already
  asserts the `.defined` half).
