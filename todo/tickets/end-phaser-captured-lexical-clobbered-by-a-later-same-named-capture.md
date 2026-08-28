# An END phaser's captured lexical is clobbered by any later closure that captures a same-named lexical

An `END` block installed inside a scope captures that scope's lexicals. Calling
*any* closure afterwards that merely **reads** a same-named lexical of a
*different* binding overwrites the phaser's captured value, so the END block runs
against the wrong variable.

## Repro (measured 2026-08-28, release build, against `raku` as the oracle)

```raku
sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;                 # a DIFFERENT binding that merely shares the name
callit { $a };             # a called closure that only READS it
```

```
mutsu: END1 (want 42): 0        raku: END1 (want 42): 42
```

The write side is worse — the closure's value leaks into the phaser:

```raku
sub callit(&c) { c() }
{ my $a = 42; END { say "END1 (want 42): ", $a.raku }; }
my $a = 0;
callit { $::('a') = 7 };   # mutsu: 7    raku: 42
```

Neither `EVAL` nor a nested `END` is required. Two negative controls pin that
down: `callit { END { 1 } }` (an END installed inside the called closure, no
capture of `a`) is **correct**, and `callit { 1 }` is correct — it is the
same-named capture that does the damage, nothing about phasers-inside-closures.

## Root cause (located, not yet fixed)

`src/vm/vm_closure_dispatch.rs`, the block just after `*self.env_mut() =
restored_env` — "After a closure returns, update captured envs of END phasers for
variables that the closure captures (and may have modified)". That refresh is
**name-keyed**: it walks `data.env`'s keys and rewrites every registered END
phaser's captured env entry of the same name. Sharing a name is not sharing a
binding, so a closure created in one scope silently rewrites a phaser captured in
an unrelated one.

The complementary half (`update_end_phaser_envs` with the `dying` set, called
just *before* the env restore) is already binding-aware in spirit — it freezes
names the frame takes with it. The post-return refresh has no such guard.

## Where it bites

`roast/S04-phasers/end.t` tests 6 and 7, but **only under `MUTSU_REAL_TEST=1`**:
the real `Test::lives-ok` is a Raku sub that *calls* the Callable it is given,
which supplies the "called closure" ingredient; mutsu's native provider does not
call it the same way, so the file passes natively.

```raku
use Test;
use MONKEY-SEE-NO-EVAL;
plan 3;
{
    { my $a = 42; END { is $a, 42, 'END1 want 42' }; }
    { BEGIN { my $a = 43; END { is $a, 43, 'END2 want 43' }; } }
}
my $a = 0;
lives-ok { EVAL 'my $x = 3; END { $a = $x * $x };' }, 'eval installs END';
```

`MUTSU_REAL_TEST=1` reports `got: '0'` for both ENDs.

## Not the same bug as the runtime-name-write one

This was found while investigating
`news/2026-08/runtime-name-write-to-outer-lexical.md` (a write through a
run-time-resolved name lost at a frame boundary) and originally suspected to
share its root cause. It does not: that fix is landed and this still reproduces,
and the negative controls above show the trigger needs neither `EVAL` nor a
runtime-resolved name — a plain read of a same-named lexical is enough.

## Why it is not a one-liner

The fix needs the phaser refresh to key on *binding identity* rather than name.
The obvious candidates are comparing against the value the phaser actually
captured (only refresh when the closure's entry is the very same `Value` /
`ContainerRef` cell) or recording, at install time, which frame's binding each
captured name came from. Both touch the END-phaser capture representation, which
`news/2026-08/end-phasers-run-in-install-order.md` also depends on — measure
before changing.
