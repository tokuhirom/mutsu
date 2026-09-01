# A `my role` declared inside `EVAL` no longer leaks out and shadows a later lexical role

Closed 2026-09-01 by the TRIAGE regeneration's repro sweep: the ticket
(`todo/tickets/eval-declared-my-role-leaks-and-shadows-a-later-lexical-role.md`,
filed 2026-08-28) no longer reproduces on `main`. No single PR is credited --
it was fixed as a side effect of the lexical-role registry work landed between
2026-08-28 and 2026-09-01 -- so this entry pins it rather than attributing it.

## What was wrong

A `my role` declared inside an `EVAL`'d string registered in the process-wide
role registry keyed by short name, so a later lexically scoped role of the
same name resolved to the EVAL's method-less version:

```raku
try EVAL 'my role R1[::T] { }; my R1 of Str $x = R1[Int].new;';
say "eval done";
{
    my role R1[::T] { method x { T } }
    say "direct: ", R1[Int].new.x.^name;   # raku: Int
}                                          # mutsu (then): No such method 'x' for invocant of type 'R1[Int]'
```

It only surfaced under `MUTSU_REAL_TEST=1`, because the real `Test.rakumod`'s
`throws-like` genuinely `EVAL`s its code string; `t/parametric-role-of-type.t`
aborted at test 5 of 14 there.

## Now

The repro prints `eval done` / `direct: Int` under both providers, and
`t/parametric-role-of-type.t` runs 14/14 under `MUTSU_REAL_TEST=1`.

Pin: `t/eval-declared-lexical-role-does-not-leak.t`. The EVAL'd role in the pin
deliberately declares no methods -- an earlier repro that gave both roles the
same method looked green while the leak was present.

Residue (cosmetic, not filed): the EVAL'd `my R1 of Str $x = R1[Int].new`
emits a mutsu-only `Useless use of constant value Int in sink context at
EVAL_0:1` warning that raku does not.
