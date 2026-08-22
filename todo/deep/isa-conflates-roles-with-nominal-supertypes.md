# `.isa()` wrongly returns `True` for role names — conflates nominal class hierarchy with role composition

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Test.rakudoc:400`).

## Root cause

Raku's `.isa(Type)` checks only the **nominal class hierarchy** (`.^mro`) — it is `False`
for a role the value merely *does*, even when that role is transitively composed by an
ancestor class. `.does(Type)` / `~~ Type` are the role-aware checks. mutsu's single
`Value::isa_check` (`src/value/types_isa.rs`) implements both `isa_check` (used directly
by `.isa`) and `does_check` (which delegates to `isa_check` for non-mixin cases) through
the *same* type-hierarchy `match` block (lines ~222-423). That match block lists several
entries that are actually **roles**, not real nominal ancestors, so `.isa` on them wrongly
returns `True`:

- `"Numeric"`, `"Real"`, `"Rational"` — roles `Cool`/`Int`/`Rat`/etc. compose, not classes
  they inherit from (confirmed via `raku -e 'say Int.^mro'` → `(Int Cool Any Mu)`, no
  `Numeric`/`Real` in the MRO).
- Likely also `"Stringy"`, `"Positional"`, `"Map"`/`"Associative"`, `"Iterable"`,
  `"Block"`/`"Routine"`/`"Code"`/`"Callable"` — these read as role names in Raku's type
  system too, though only `Numeric`/`Real`/`Rational` were directly verified against real
  `raku` here.

## Minimal repro

```raku
say 42.isa(Numeric);   # raku: False (role, not in Int's MRO)  mutsu: True
say 42.isa(Real);      # raku: False                            mutsu: (untested but same shape)
say 42.isa(Cool);      # raku: True (Cool IS in Int's MRO)      mutsu: True (correct)
```

- `raku`: `False`
- `mutsu` (`target/debug/mutsu`): `True`

The doc's own example (`Type/Test.rakudoc:400`) relies on this distinction directly:
`say 42.isa(Numeric)` is documented as `False`, contrasted with `isa-ok 42, Numeric` (a
`Test` assertion that uses the role-aware `~~`/`does` check and correctly says `ok`).

## Why this is `todo/deep`, not a shallow slice

- `isa_check` is the single shared implementation behind BOTH `.isa` (should be
  nominal-only) and `.does`/`~~` (should be role-aware) — `does_check` explicitly falls
  through to `isa_check` for the non-mixin case (`src/value/types_isa.rs:449`). Simply
  removing the role entries from the match would break `.does(Numeric)`/`42 ~~ Numeric`,
  which must stay `True`.
- A correct fix needs to bifurcate the current single match block into two: a nominal-only
  table (real class-hierarchy ancestors: `Any`, `Mu`, `Cool`, `Dateish`, the `Exception`
  family, `ObjAt`, `Pod::Block`, ...) consulted by `.isa`, and a role-table (`Numeric`,
  `Real`, `Rational`, `Positional`, `Associative`/`Map`, `Iterable`, `Stringy`, `Block`
  family, ...) consulted only by `.does`/`~~`/smart-match. Several entries in the current
  match (`SetHash`/`BagHash`/`MixHash`, `Seq`/`List`, `HyperSeq`/`RaceSeq`) may be
  legitimately nominal (they're concrete classes, not roles) and should stay shared — this
  needs a careful per-entry audit against real `raku -e 'say TYPE.^mro'` /
  `say TYPE.^roles` output, not a blanket split.
- Touches a very hot, widely-used primitive (`isa_check`/`does_check` back essentially
  every `.isa`, `.does`, `~~`, and multi-dispatch type-constraint check in the
  interpreter), so the blast radius of getting the split wrong is large — this needs
  `make test` + a full roast run to validate, not a quick local check.

## Affected files (starting point)

- `src/value/types_isa.rs` (`isa_check`, `does_check`)
