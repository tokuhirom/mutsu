# A statement-modifier `if`/`unless` on `my`/`our constant` no longer loses the value

```raku
my constant $w = 11 if True;
say $w;
# raku:  11
# mutsu (before): (Any)
```

`raku` treats a `constant` declaration as bound at compile time, independent
of any runtime statement modifier it is textually written with — the
initializer runs and the constant is bound *unconditionally*, even under an
`if False`:

```raku
my constant $w = 11 if False;
say $w;   # raku: 11 (confirmed with real raku -- no warning either)
```

## Root cause

`parse_statement_modifier` (`src/parser/stmt/modifier.rs`) special-cases a
`my`/`our` scalar/array/hash `VarDecl` carrying a statement modifier
(`try_split_decl_modifier`): since Raku declarations always take effect at
compile time and only the *initializer* should be gated, it splits `my $x =
INIT if COND` into an always-run bare declaration plus a conditional
`Assign`. That split is correct for an ordinary mutable variable, but a
`VarDecl` carrying the `__constant` custom trait is not an ordinary variable
— the compiler's constant-binding path evaluates and registers the
initializer expression directly off the `VarDecl` at declaration time. After
the split, the declaration half carried a default placeholder initializer
(`Nil`) and the *real* initializer (`11`) was demoted to a plain runtime
`Assign` to `$w` — which a constant binding does not observe, since a
constant is not a normal writable container. The result: `$w` bound at
`Nil`→`Any`, and the later conditional assignment silently had no effect.

## Fix

`try_split_decl_modifier` now checks for the `__constant` trait first and,
when present, returns the original (unsplit) declaration verbatim, dropping
the modifier's condition entirely — matching `raku`'s own observed behavior
of always evaluating a `constant`'s initializer regardless of the modifier.
This applies uniformly to `if`/`unless`, scalar/array/hash sigils, and
`my`/`our` scope.

Regression test: `t/constant-statement-modifier-value-kept.t` (9 assertions,
all verified against real `raku`), including a guard that an *ordinary*
(non-`constant`) `my` declaration is still genuinely gated by the modifier —
the constant-specific bypass does not leak into normal variable declarations.

## Found but out of scope

A **separate, pre-existing** parser bug (confirmed via `git stash` to
reproduce identically before this fix): a *bare* (no `my`/`our`) `constant`
declaration followed by an `if`/`unless` modifier fails to parse entirely
(`===SORRY!=== Missing block`), even though `raku` accepts it and evaluates
the constant the same way. Filed as
`todo/tickets/bare-constant-if-modifier-missing-block.md`.
