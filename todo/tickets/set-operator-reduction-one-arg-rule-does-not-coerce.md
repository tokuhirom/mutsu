# A one-operand set reduction returns the operand instead of coercing it

Found 2026-09-06 while sweeping the neighbourhood of the Mix-weight
numeric-tower fix. Pre-existing and uniform across every set operator — it is
not specific to the `[(+)]` spelling that fix newly enabled.

## Repro

```raku
say ([(|)] (a => 2).Bag).raku;
# raku:  ("a"=>2).Bag
# mutsu: :a(2)
```

Identical for `[(&)]`, `[(.)]`, `[(^)]`, `[(-)]` and `[(+)]`.

The scan form has the same root cause in its *first* element:

```raku
say ([\(|)] <a>, <a>).raku;
# raku:  $((Set.new("a"), Set.new("a")).Seq)
# mutsu: ("a", Set.new("a")).Seq
```

mutsu's first scan element is the bare `"a"`; Rakudo's is `[(|)]("a")`, i.e.
`Set.new("a")`.

## Root cause

Raku's reduction one-arg rule is `[op]($x)` = `op($x)`, and for a set operator
that unary application is a *coercion* — to `Set` for `(|)`/`(&)`/`(^)`/`(-)`,
to `Bag` for `(+)`/`(.)`. mutsu instead returns the single operand untouched.

Two sites, both in `src/vm/vm_misc_reduction_exec.rs`:

- the `list.len() == 1` block near the top flattens a single list-like operand
  into elements, so `[(|)] (a => 2).Bag` ends up as the one element `:a(2)` and
  falls through as itself;
- the scan branch already special-cases this for two operator families —
  `Z~`/`X~` wrap in a `Seq`, `minmax` builds `x..x` — and the set operators need
  a third arm there.

`runtime::reduction_identity_opt` already knows the right zero for each set
operator (`Value::bag(...)` for `(+)`/`(.)`, and so on in
`src/runtime/utils/type_misc.rs:308`), so the coercion target per operator is
already written down and can be reused rather than re-derived.

## Why it was not folded into the numeric-tower fix

It is a different rule (arity handling in the reduction) from that fix (how two
weights combine), it touches the shared reduction entry point rather than the
set operators, and it is wrong identically for operators that fix did not
change — so it wants its own test and its own diff.
