# `my &f = ...` does not shadow an outer `sub f` for a by-name call

A `my &f` binding is an ordinary lexical and shadows any outer routine of the
same name. mutsu resolves a by-name call to the routine registry first, so the
binding is ignored and the outer routine is called instead — usually with the
wrong arity, so it surfaces as a confusing "Too few positionals passed".

## Minimal repro

```raku
sub tester($a, $b) { say "outer $a $b" }
tester(1, 2);

{
    my &tester = -> $x { say "lexical $x" };
    tester(9);
}
tester(3, 4);
```

```
raku                     mutsu
outer 1 2                outer 1 2
lexical 9                Too few positionals passed; expected 2 arguments but got 1
outer 3 4                  in block <unit> at ...:4
```

Calling through the variable (`&tester($x)` / `$tester(9)` shapes) works; it is
specifically the bare-name listop/parenthesised call that goes to the registry.

## Why it matters

This is what `roast/S04-statements/given.t` reaches after
`news/2026-08/eval-sub-shadows-a-registered-routine.md`. Its
`given/when with explicit conditions` subtest does

```raku
my &test-given = produce-tester $condition;   # produce-tester EVALs a fresh sub
test-given topic, $desc, :$match;
```

while an earlier subtest's `my sub test-given(Mu \topic, Mu \condition, $message, :$match)`
is still in the registry. The call resolves to that 3-positional routine and
dies with "Too few positionals passed; expected 3 arguments but got 2", losing
the whole 3-subtest group.

## Why it is not a one-liner

The call-resolution order is shared by every by-name call site, and mutsu
deliberately prefers the registry in several places (imported routines, `our`
routines, multi dispatch, the native-builtin guards of
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`). The rule to
implement is Raku's: a *lexical* `&name` binding visible at the call site wins
over a package/registry routine of that name, and only falls through when there
is none. Deciding "visible at the call site" has to work at compile time where
possible (the compiler knows the enclosing `my &name` declarations) rather than
by probing `env` at run time, or the fast call paths regress.

Related: `todo/deep/...`-style leak that makes this hit more often than it
should — a `my sub` declared inside a block invoked as a *callable* stays
visible after the block (`::('&tester').defined` is `True` in mutsu, `False` in
raku), so the outer routine that loses the contest is often one that should not
exist at that point at all.
