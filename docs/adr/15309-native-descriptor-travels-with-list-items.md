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
| `$l[0].VAR.^name` | `UIntLexRef`, i.e. the variable's own native container |

So it is not a "native tag on a value". A List literal's items *are* the variables' containers (that is also why
`my $x = 1; my $l = ($x, 7); $x = 2; $l[0]` is `2`), and the dispatcher looks at the container behind an argument
that is an element read (`@a[0]` passes the element container, like an `is raw` return). Copying into an `Array`
(`my @arr = ...`) gives fresh `Scalar` containers, which lose it.

A plain `Int` never satisfies a `byte` candidate of a multi (`n16(1, 0)` is refused); a single non-multi
`sub f(byte $a)` accepts `f(1)`.

### 1.3 What mutsu does today (measured on `main` 2026-10-31)

- `ContainerCell` already holds the container's `of`-type (`constraint`) and `default`; a typed scalar's `of` is
  documented to travel through aliases. The dispatcher's `Value::varref` argument carries the source *variable's*
  declared type, which is how `f($b)` picks `byte`.
- A List literal does alias its variables (`2` in the example above), but the element read hands the dispatcher the
  dereferenced value: `($t,)[0].VAR.of` for `my Int $t` dies with `No such method 'of' for invocant of type 'Int'`
  (rakudo: `Int`), `$l[0].VAR` of a plain `$x` is a `Scalar` (correct) but of a `byte` variable it is not the native
  container.
- A literal `Int` argument is accepted by a multi's `byte` candidate (`m(1, 0)` runs; rakudo refuses it). This is
  looser than rakudo and unrelated to the symptom, and is left alone here.

## 2. Decision (proposed)

1. **An element read used as a call argument keeps its container.** When an index expression (`@a[i]`, `%h<k>`,
   `$l[i]`) is an argument, the dispatcher is given the element's `ContainerRef` (as `is raw` already does for a
   return value), and `unwrap_arg_for_dispatch` takes the variable type from the cell's `CellConstraint` when the
   argument has no `varref` type. No new `Value` variant and no value-level native tag.
2. **A List literal stores each variable's own cell**, not a dereferenced copy, so the cell (and its `of`-type)
   is what `@a[0]` later finds. Aliasing already works; this makes the descriptor ride on it.
3. **Array copies keep losing it**, matching `my @arr = ($b, $b); @arr[0]` → `Int`.

### 2.1 Acceptance

`t/` regression pinning every row of the §1.2 table, plus the Net::BGP `n16` reduction; Net::BGP's
`t/00-conversions.t` matching `raku`.

## 3. Alternatives considered

- **A `byte`/`uint8` flag on the boxed `Int` value.** Rejected: rakudo's rows (`@arr[0]` is `Int`, `id($b)` is
  `Int`) show the property belongs to the container, not the value; a value-level tag would also survive copies that
  must drop it, and would need a `Value` variant on a hot path.
- **Accept any in-range `Int` for a `byte` candidate** (making the symptom disappear). Rejected: it changes dispatch
  for `multi (byte)` / `multi (Int)` pairs (`m(5)` would stop choosing `Int`) and does not match rakudo.
- **Special-case Net::BGP's recursion.** Rejected (AGENTS.md: no distribution-specific code).

## 4. Consequences and risks

- Step 1 touches the argument path of every call that passes an element: it must stay free of an extra allocation
  or lock for the common case (an `Int` element in an untyped container). A cheap guard is "the cell has a
  `CellConstraint`"; untyped cells pay one `Option` check.
- Step 2 changes what a List literal holds for a variable argument; the existing aliasing tests
  (`t/` "list holds the variable") are the guard.
- `.VAR` of an element would become more faithful (`UIntLexRef`-like names are *not* in scope; `.VAR.of` and
  `.VAR.^name` for native variables are a follow-up).

## 5. Open questions

1. Does mutsu already store the cell for `($x, 7)` and only dereference it on `AT-POS`, or does the List hold a
   `varref` that is resolved at read time? The implementation slice starts by answering this with
   `rust-gdb` on `($t,)[0].VAR.of` (it decides whether step 2 is a change or already true).
2. Should the literal-`Int`-accepted-by-`byte` looseness (§1.3, third bullet) be tightened in the same slice? Proposal:
   no, separate issue, because several batteries may rely on it.
