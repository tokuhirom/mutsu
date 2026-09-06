# Native methods still without a declared set of accepted named arguments

**Narrowed 2026-09-07.** The mechanism this file asked for now exists:
[ADR-0070](../../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md),
`src/builtins/accepted_nameds.rs`, `scripts/native-method-adverb-survey.raku`
and `t/native-method-accepted-nameds.t`. Every row of the original measured
table is fixed. What is left is the residue of the same survey, which needs no
new design — only more rows.

## What shipped

`native_method_accepted_nameds(method) -> Option<&'static [&'static str]>` is
consulted at the three places the builtin dispatch layer is entered
(`try_native_method_raw`'s arity cascade, its interpreter-side twin, and
`dispatch_method_by_name_1/2/3`). A named argument outside a *declared*
method's set is dropped before an arity is chosen; a method the table does not
mention is untouched. 29 methods are declared, from a Rakudo signature survey.
`first` gained the `X::Adverb` validation Rakudo does instead (mirroring the
`grep` handler), in both the method and the sub form.

A 2 600-probe sweep over `native_method_row_table.rs` went from 754
not-named-blind probes to 422, with no probe losing named-blindness.

## What is left

### 1. Methods still undeclared, with a measured wrong answer

- **`add` / `remove` on the mutable QuantHashes.** `BagHash.new(1,2,2).add(1,
  :zzz)` dies with "Too many positionals passed; expected 2 arguments but got 3"
  where raku answers `Nil`. Rakudo's survey says these accept no named at all,
  so the row itself is trivial — but the failure does *not* come from either
  arity cascade or from `dispatch_method_by_name_*`, so adding the row alone
  does not fix it. Find the handler that raises that arity error (it looks like
  a class-registered native method with a declared signature) and route it
  through the same declaration. `grab` is in the same family.
- **`base(:no-trailing-zeroes)`** is declared, and correctly so, but not
  implemented: `0.5.base(10, 5, :no-trailing-zeroes)` answers `"0.50000"` where
  raku answers `"0.5"`. Purely a missing feature now; the adverb reaches the
  implementation.

### 2. Methods whose accepted set Rakudo answers only through `%_`

`scripts/native-method-adverb-survey.raku` reports `**SLURPY**` and nothing else
for a routine that reads its adverbs out of `%_` rather than declaring
parameters — `grep`, `first`, `subst`, `trans`, `reduce`, `produce`, `keys`,
`values`, `kv`, `pairs`, `list`, `Array`. For those the survey is a *lower
bound*, so they are deliberately absent from the table. Each needs its accepted
set confirmed by hand against `raku-doc/doc/Type/` before it can be declared.
(Measured 2026-09-07: none of them currently answers a `:zzz` probe wrongly, so
this is hardening, not a live bug.)

### 3. Unrelated divergences the sweep surfaced

Not named-argument bugs — the *plain* call already differs from raku — but worth
their own tickets if anyone picks them up:

- `4.roots(2)` answers `(2e0, -2e0)` where raku answers the two complex roots
  `(<2+0i>, <-2+2.4e-16i>)`.
- `(1,2,3).rotor("b")` answers `().Seq` where raku dies "cannot unbox to a
  native integer"; `(1,2,3).AT-POS("b")` answers `Nil` where raku dies.
- `(1,2,3).splice(1, 1)` reports `X::Immutable` where raku reports
  "Cannot resolve caller".

## Repro for the residue

```
raku -e 'say BagHash.new(1,2,2).add(1, :zzz).raku; say 0.5.base(10, 5, :no-trailing-zeroes)'
# Nil / 0.5
./target/debug/mutsu -e 'say BagHash.new(1,2,2).add(1, :zzz).raku; say 0.5.base(10, 5, :no-trailing-zeroes)'
# dies / 0.50000
```

## Affected files

- `src/builtins/accepted_nameds.rs` (the table — add rows here)
- `scripts/native-method-adverb-survey.raku` (regenerates the evidence)
- `t/native-method-accepted-nameds.t` (pins every declared row, and passes
  under `raku` unmodified)
- whichever handler raises the `add`/`remove` arity error (not yet located)
