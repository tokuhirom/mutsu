# ADR-15309: A native variable's descriptor travels with the container its List item shares

- **Status**: Proposed
- **Date**: 2026-10-31
- **Deciders**: tokuhirom, Claude
- **Issue**: [#15309](https://github.com/tokuhirom/mutsu/issues/15309) (Net::BGP `Net::BGP::Conversions`,
  about 15 of its 25 test files)
- **Related**: [ADR-0049](0049-nil-decays-to-the-container-default-at-the-element-store.md) (the `default` that lives on
  `ContainerCell`, as the `of`-type does: `src/value/mod.rs`, `CellConstraint`). Dispatch ranking by source variable
  type: `src/runtime/multi_type_walk.rs` (`unwrap_arg_for_dispatch`) and `src/runtime/dispatch_candidates.rs`
  (`type_hierarchy_distance_with_var_type`).

## 1. Context

### 1.1 The symptom

```raku
multi sub n16(@a where @a.elems == 2 --> Int) { n16(@a[0], @a[1]) }
multi sub n16(byte $a, byte $b --> Int)       { $a × 2⁸ + $b }
my byte $b0 = 0; my byte $b1 = 1;
say n16(($b1, $b0));     # rakudo: 256     mutsu: Cannot resolve caller n16(Int:D, Int:D)
```

### 1.2 What rakudo does (measured with `raku`, 2026.09)

A `multi` with `(byte $a)` and `(Int $a)` candidates, `my byte $b = 1`:

| argument expression | candidate chosen |
| --- | --- |
| `$b` | `byte` |
| `($b, 5)[0]`, `my $l = ($b, $b); $l[0]` | `byte` |
| `($b, 5)[1]` | `Int` |
| `my @bound := ($b, $b); @bound[0]` | `byte` |
| `my @arr = ($b, $b); @arr[0]`, `[$b][0]`, `%h<a>` after `%h = a => $b` | `Int` |
| `id($b)` for `sub id($x) { $x }`, and `idr($b)` for `sub idr(\x) { x }` | `Int` |
| `$l.head`, `$l.first` | `byte` |
| `my \e = $l[0]; e` | `byte` |
| `for $l.list <-> $x { $x }` | `byte` |
| `for $l.list -> $x { $x }` | `Int` (a read-only parameter unwraps the container) |
| `$l[0].VAR.^name` | `UIntLexRef`, i.e. the variable's own native container |

So it is not a "native tag on a value", and it is not about subscripts. The rule is general:

1. A List's items **are** the variables' containers (that is also why `my $x = 1; my $l = ($x, 7); $x = 2; $l[0]` is
   `2`).
2. An accessor that returns raw (`[]`, `.head`, `.first`, `AT-POS`, a `\e` binding) hands the container on as it is.
3. The multi dispatcher reads the `of`-type of a *container* argument.
4. Binding to a **read-only** parameter (`->`, a plain sub parameter, `$x` in `id($x)`) unwraps the container to its
   value, which is why `id($b)` and `for ... -> $x` give `Int`, while `<->`, `\e` and `is raw` keep it.

Copying into an `Array` (`my @arr = ...`) creates fresh `Scalar` containers, which drop the descriptor.

A plain `Int` never satisfies a `byte` candidate of a multi (`n16(1, 0)` is refused); a single non-multi
`sub f(byte $a)` accepts `f(1)`.

### 1.3 What mutsu does today (measured on `main` 2026-10-31)

- `ContainerCell` can hold an `of`-type (`constraint`, a `Mutex<Option<Box<CellConstraint>>>`) and a `default`, and a
  typed scalar's `of` is documented to travel through aliases. **That is not established for a native variable**: the
  `declared_untyped` comment (`src/value/mod.rs`, next to the `name` field) says that "no `of`-type" is what an untyped
  variable's cell *and* a native variable's cell both look like. So the premise that the cell of a `my byte $b` carries
  `byte` is something this ADR has to make true, not assume (§2 step 2, §5).
- The dispatcher's `Value::varref` argument carries the source *variable's* declared type, which is how `f($b)` picks
  `byte`; there is no equivalent for an argument that is a container reached through an accessor.
- A List literal does alias its variables (`2` in the example above), but the element read hands the dispatcher the
  dereferenced value: `($t,)[0].VAR.of` for `my Int $t` dies with `No such method 'of' for invocant of type 'Int'`
  (rakudo: `Int`), `$l[0].VAR` of a plain `$x` is a `Scalar` (correct) but of a `byte` variable it is not the native
  container.
- A literal `Int` argument is accepted by a multi's `byte` candidate (`m(1, 0)` runs; rakudo refuses it). This is
  looser than rakudo and unrelated to the symptom, and is left alone here.

## 2. Decision (proposed)

1. **State the rule, not a syntax case.** A call argument that is a container (a `ContainerRef`) is dispatched by
   the container's `of`-type; a container argument is what a raw-returning accessor produces (§1.2 items 2-3). The
   dispatcher change is in `unwrap_arg_for_dispatch` (`src/runtime/multi_type_walk.rs`): when the argument has no
   `varref` type, take the type from the cell, and only then dereference it. Which accessors return the container
   rather than the value is decided accessor by accessor against §1.2 (`[]`, `.head`, `.first`, `\e`, `<->`), never by
   the call-site syntax.
2. **Register the `of`-type on a native variable's cell.** Where a native variable (`my byte $b`) is promoted to a
   shared cell (the `capture_var_cell` / list-item promotion sites), set its `CellConstraint` to the declared native
   type, so the cell is distinguishable from an untyped variable's. This is the step that makes the cell, not the
   `varref`, carry `byte`. `declared_untyped` stays as it is (it answers a different question: whether a cell may be an
   atomic native-integer one).
3. **A List literal stores each variable's own cell**, not a dereferenced copy, so the cell is what an accessor later
   finds. Aliasing already works; this makes the descriptor ride on it.
4. **Binding to a read-only parameter keeps unwrapping**, so `->`, a plain parameter and `id($b)` still see `Int`;
   `<->`, `\e` and `is raw` keep the container (§1.2 rows 4-7).
5. **Array copies keep losing it** (`my @arr = ($b, $b); @arr[0]` is `Int`).

### 2.1 Acceptance

`t/` regression pinning every row of the §1.2 table (including `.head`, `.first`, `my \e = $l[0]`, `<->` versus `->`,
the `Array` copy, `id($b)` and `$l[1]` of `($b, 5)`), plus the Net::BGP `n16` reduction; Net::BGP's
`t/00-conversions.t` matching `raku`. A verification item before the dispatcher change: `($t,)[0].VAR.of` for
`my Int $t` and for `my byte $b` answers the declared type (it dies today for `Int`, §1.3).

## 3. Alternatives considered

- **A `byte`/`uint8` flag on the boxed `Int` value.** Rejected: rakudo's rows (`@arr[0]` is `Int`, `id($b)` is
  `Int`) show the property belongs to the container, not the value; a value-level tag would also survive copies that
  must drop it, and would need a `Value` variant on a hot path.
- **Accept any in-range `Int` for a `byte` candidate** (making the symptom disappear). Rejected: it changes dispatch
  for `multi (byte)` / `multi (Int)` pairs (`m(5)` would stop choosing `Int`) and does not match rakudo.
- **Special-case Net::BGP's recursion.** Rejected (AGENTS.md: no distribution-specific code).

## 4. Consequences and risks

- **Cost on the hot path.** Reading a cell's `of`-type today is `lookup_container_constraint`, which locks the
  `constraint` mutex and clones a `String` (`src/value/mod.rs`). Calling it for every container argument would put a
  lock and an allocation on every call that passes an element. The dispatch lookup therefore needs an early-out: gate it
  on a process-wide flag that is set when a native variable's cell registers its `of`-type (the pattern of
  `CELL_METADATA_SEEN` / `cell_metadata_possible()`, but a separate flag, so unrelated `is default` users do not
  switch the lookup on), and have it return a borrowed/interned type rather than a `String`. A program that never
  declares a native variable pays one relaxed atomic load per container argument.
- Step 3 changes what a List literal holds for a variable argument; the existing aliasing tests are the guard.
- Step 2 changes how a native variable's cell looks to every consumer of `constraint` (element type checks, `.VAR.of`),
  so each consumer is audited for "native" meaning "no boxing check": a store through such a cell must still reject
  an out-of-range value.
- `.VAR.^name` for native variables (`UIntLexRef`) is out of scope.

## 5. Open questions

1. Does mutsu already store the cell for `($x, 7)` and only dereference it on `AT-POS`, or does the List hold a
   `varref` resolved at read time? The implementation slice starts by answering this with `rust-gdb` on
   `($t,)[0].VAR.of`; it decides whether step 3 is a change or already true.
2. Which of mutsu's raw-returning accessors already hand the container on, and which dereference? The §1.2 rows are
   the list to check them against.
3. The literal `Int` accepted by a multi's `byte` candidate (`m(1, 0)` runs; rakudo refuses it) is looser than
   rakudo. It is left alone here, because tightening it is a separate behaviour change; whether anything depends on it
   has not been measured, and it should be measured before that issue is worked, not assumed.
