# `.isa()` no longer conflates roles with nominal supertypes

`.isa(Type)` in Raku checks only the **nominal class hierarchy** (`.^mro`) — it
must be `False` for a role a value merely *does* (composes), even when an
ancestor class composes that role. `.does(Type)` / `~~ Type` are the
role-aware checks. mutsu's single `Value::isa_check`
(`src/value/types_isa.rs`) used to implement both `isa_check` (backing
`.isa()`) and `does_check` (backing `.does()`/`~~`, which fell through to
`isa_check` for the non-mixin case) through the *same* type-hierarchy `match`
block, so it conflated the two: `42.isa(Numeric)` wrongly returned `True` in
mutsu (raku: `False`, since `Int.^mro` is `(Int Cool Any Mu)` — no `Numeric`).

## The fix

The match block was audited entry by entry against real `raku -e 'say
TYPE.^mro'` / `say TYPE.^roles(:transitive)` output (not a blanket guess from
names) and split into two tables:

- `isa_nominal_hierarchy` — real class-hierarchy ancestors only (`Any`, `Mu`,
  `Cool`, `Capture`, `FatRat`, `Int` for `Bool`, `Block`/`Routine`/`Code`,
  `Method`, the `Exception` family, `List`, `Map`, `ObjAt`, `Pod::Block`,
  `Pod::Config`, `SetHash`/`BagHash`/`MixHash` identity checks). Consulted by
  `isa_check` and, as the "or is a real ancestor" half of `does = isa OR
  does-role`, by `does_check`.
- `does_role_hierarchy` — role names only (`Numeric`, `Real`, `Rational`,
  `Dateish`, `Stringy`, `Callable`, `Positional`, `Associative`, `Iterable`).
  Consulted only by `does_check`.

Both functions now share one prefix (wrapper unwrapping for `VarRef`,
`Scalar`, `ContainerRef`, a forced `LazyThunk`, `HashEntryRef`, and `Mixin`) via
a new private `isa_or_does_check(type_name, allow_roles)`, so the two tables
stay in sync without duplicating that logic.

The per-entry audit turned up several conflation bugs beyond the ticket's
original `Numeric`/`Real`/`Rational`/`Dateish`/`Stringy`/`Positional`/
`Associative`/`Iterable`/`Callable` list:

- `Capture.new.isa(Cool)` was `True` (bug) — only `Match` (whose `.^mro` is
  `(Match Capture Cool Any Mu)`) is nominally `Cool`; a bare `Capture`'s
  `.^mro` is `(Capture Any Mu)`, no `Cool`.
- `(1=>2).isa(Map)`, `Set.new.isa(Map)`, `Capture.new.isa(Map)` were all
  `True` (bug) — `Map` is a real ancestor of `Hash` only (`Hash.^mro` is
  `(Hash Map Cool Any Mu)`); `Pair`/`Set`/`Bag`/`Mix`/`Capture` merely compose
  the `Associative` role, which is not enough for `.isa(Map)` in real raku.
- A lazy array (`my @a = lazy 1,2,3`) `.isa(Seq)` was `True` (bug) — its true
  nominal type is `Array`/`List` (`.isa(Seq)` is `False` in real raku for this
  shape; a genuine `map`/`grep` `Seq` result is unaffected and already
  correctly `isa(Seq)`/not `isa(List)` via mutsu's separate `ValueView::Seq`).
- `HyperSeq`/`RaceSeq` were granted `isa(List)`/`isa(Seq)` via the old
  combined arm — real raku's `.^mro` for both is just `(HyperSeq/RaceSeq Any
  Mu)`, no `List`/`Seq` ancestry at all.

`X::OS` stays in the nominal table unchanged: real raku models it as a role,
but mutsu's own exception hierarchy instantiates it directly as a class
(`native_proc_async.rs`) and the existing check is an exact `class_name`
equality that's effectively dead for anything else, so touching it would add
risk without a real behavior change.

## Verification

- `.isa()` for user-defined classes/instances was already routed through a
  separate, correct `class_mro`-based registry lookup
  (`methods_instance_ops.rs`), not through this match block at all — so this
  fix's blast radius is scoped to mutsu's built-in/primitive value types
  (`Int`, `Str`, `Array`, `Hash`, `Pair`, `Range`, `Sub`, ...), not user class
  hierarchies.
- `~~`/`.does()` for most cases are already routed through the separate,
  more-authoritative `type_matches_value` dispatcher
  (`runtime/types/type_matching.rs`), which only calls `does_check` for names
  resolved through the role registry — so `does_check`'s behavior (and thus
  most `~~`/`.does()` call sites) is unchanged by this fix except where it was
  called directly on a value.
- New regression test `t/isa-does-role-nominal-split.t` (46 assertions)
  covers every changed entry, each verified against real `raku` output.
- `make test` (3354 files, 31497 assertions) passes with no regressions.
- Roast files with `.isa(` under `S02-types`, `S02-literals`, `S12-class`,
  `S12-attributes`, `S12-introspection` (`mixhash.t`, `sethash.t`,
  `baghash.t`, `allomorphic.t`, `subset-6e.t`, `inheritance.t`, `literal.t`,
  `basic.t`, `instance.t`, `meta-class.t`) all still pass.

One pre-existing, unrelated gap surfaced during verification: mutsu's
`.hyper()` builtin does not produce a genuine `ValueView::HyperSeq` — it is
backed by `ValueView::Array` internally, and only `.^name`/`.WHAT` fake the
"HyperSeq" display name through a separate mechanism. Left out of scope; not
part of this fix.
