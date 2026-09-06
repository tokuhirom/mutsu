# A one-operand set reduction now coerces, and a QuantHash is one operand

Two defects, one root. Raku's reduction one-arg rule is `[op]($x)` == `op($x)`,
and for a set operator that unary application is a *coercion*. mutsu returned
the single operand untouched:

```raku
say ([(|)] (a => 2).Bag).raku;   # raku: ("a"=>2).Bag   mutsu (before): :a(2)
say ([(|)] 3).raku;              # raku: Set.new(3)     mutsu (before): 3
```

The `:a(2)` in that first line is the second, deeper defect showing through: a
`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash` does **not** do `Iterable` in
rakudo (`Set ~~ Iterable` is `False`), so it is *one* operand — but mutsu ran it
through `value_to_list` and folded over its decomposed pairs. That was wrong for
every reduction, not only the set operators:

| Program | raku | mutsu (before) |
|---|---|---|
| `[~] Set.new("a","b")` | `"a b"` | `"a\tTrueb\tTrue"` |
| `[+] bag(1,1,2)` | `3` | `0` |
| `[,] Set.new("a","b")` | `(Set.new("a","b"),)` | `(:a, :b)` |
| `[min] bag(1,1,2)` | the `Bag` | `1 => 2` |

## What changed

A QuantHash operand is now held whole in `exec_reduction_op`, exactly as a
`Blob` already was and for the same reason, and the one-arg rule is applied to
it. The deref matters: a `$`-lexical read yields a `ContainerRef`, which a view
match would not see through.

The set operators' one-argument candidates are transcribed in the new
`src/vm/vm_misc_reduction_setop.rs`, per operator, because rakudo's candidate
set genuinely differs per operator:

- `(|)` / `(&)` / `(^)` — a QuantHash passes through untouched (a `BagHash`
  stays a `BagHash`); anything else becomes a `Set`.
- `(-)` — keeps the tower level but always yields the *immutable* spelling, so
  `[(-)] <a b>.SetHash` is a `Set`.
- `(+)` — baggy: promotes `Set`-level operands to `Bag`, also immutable.
- `(.)` — baggy but *preserves* mutability, so a `SetHash` becomes a `BagHash`.

The scan form applies the same rule to its first element, which is what the
existing `Z~`/`X~` and `minmax` arms beside it already do:
`[\(|)] <a>, <a>` is `(Set.new("a"), Set.new("a"))`, not `("a", Set.new("a"))`.

Two neighbouring rules were completed while the operand became visible:

- `infix:<~>`'s one-arg candidate (`multi sub infix:<~>(Any \a) { a.Str }`) was
  missing, so `[~] 5` was the `Int` 5 rather than the `Str` `"5"`. `Blob` keeps
  its own identity candidate.
- `apply_reduction_op`'s `+` arm did a *baggy sum* when both operands coerced to
  bag counts, so `[+] Set.new("a"), Set.new("b")` answered `("a"=>1,"b"=>1).Bag`
  where the very same `Set.new("a") + Set.new("b")` infix answered `2`. The
  baggy sum is `(+)`, a different operator; the arm is now plain numeric
  addition and the reduction agrees with the infix it folds.

Pinned by `t/set-reduction-one-arg.t` — 45 assertions, every one measured
against raku v2026.07 first, covering all six operators across
`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash`/scalar operands, the scan form,
and multi-operand folds staying unchanged.

One neighbouring defect surfaced and was filed rather than fixed here:
`Set ~~ SetHash` is `True` in mutsu and `False` in raku
(`todo/tickets/set-smartmatches-its-mutable-counterpart.md`) — a smartmatch
defect, not a reduction one.
