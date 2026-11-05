# ADR-15309: A native variable's descriptor travels with the container its List item shares

- **Status**: Accepted (2026-11-01)
- **Date**: 2026-10-31
- **Deciders**: tokuhirom, Claude
- **Issue**: [#15309](https://github.com/tokuhirom/mutsu/issues/15309) (Net::BGP `Net::BGP::Conversions`,
  about 15 of its 25 test files)
- **Related**: [ADR-0049](0049-nil-decays-to-the-container-default-at-the-element-store.md) (the `default` that lives on
  `ContainerCell`, as the `of`-type does: `src/value/mod.rs`, `CellConstraint`). Dispatch by source variable type:
  `src/runtime/multi_type_walk.rs` (`unwrap_arg_for_dispatch`), `src/runtime/types/args_matching.rs`
  (`native_dispatch_arg_matches`), `src/vm/vm_call_method_compiled_cache.rs` (`multi_arg_type_keys_impl`).
  Element arguments: `src/vm/vm_subscript_invocant_ref.rs` (`exec_index_arg_ref_op`).

## 1. Context

### 1.1 The symptom

```raku
multi sub n16(@a where @a.elems == 2 --> Int) { n16(@a[0], @a[1]) }
multi sub n16(byte $a, byte $b --> Int)       { $a × 2⁸ + $b }
my byte $b0 = 0; my byte $b1 = 1;
say n16(($b1, $b0));     # rakudo: 256     mutsu: Cannot resolve caller n16(Int:D, Int:D)
```

### 1.2 What rakudo does (measured with `raku`, 2026.09)

A `multi` with `(byte $a)` and `(Int $a)` candidates, `my byte $b = 1`, `my $l = ($b, $b)`:

| argument expression | candidate chosen |
| --- | --- |
| `$b` | `byte` |
| `($b, 5)[0]`, `$l[0]`, `$l.head`, `$l.first`, `$l.AT-POS(0)` | `byte` |
| `($b, 5)[1]` | `Int` |
| `my @bound := ($b, $b); @bound[0]` | `byte` |
| `sub k(@a) { m(@a[0]) }; k(($b, 2))` (the Net::BGP shape) | `byte` |
| `my \e = $l[0]; e` | `byte` |
| `for $l.list <-> $x { $x }` | `byte` |
| `for $l.list -> $x { $x }` | `Int` (a read-only parameter unwraps the container) |
| `my @arr = ($b, $b); @arr[0]`, `[$b][0]`, `%h<a>` after `%h = a => $b` | `Int` |
| `id($b)` for `sub id($x) { $x }`, `r($b)` for `sub r(\x) is raw { x }`, `$l[0]<>` | `Int` |
| `my uint8 $u; ($u,)[0]` against the same pair | `byte` |
| `$l[0].VAR.^name` | `UIntLexRef`, the variable's own native container |

So it is not a "native tag on a value", and it is not about subscripts. The rule is general:

1. A List's items **are** the variables' containers (that is also why `my $x = 1; my $l = ($x, 7); $x = 2; $l[0]` is
   `2`).
2. An accessor that returns raw (`[]`, `.head`, `.first`, `AT-POS`, a `\e` binding) hands the container on as it is.
3. The multi dispatcher reads the `of`-type of a *container* argument.
4. Binding to a **read-only** parameter (`->`, a plain sub parameter, `$x` in `id($x)`) unwraps the container to its
   value, which is why `id($b)` and `for ... -> $x` give `Int`, while `<->` and `\e` keep it.

Copying into an `Array` (`my @arr = ...`) creates fresh `Scalar` containers, which drop the descriptor.

A plain `Int` never satisfies a multi whose only candidate takes `byte` (`multi n(byte $a); n(1)` is a compile-time
"will never work"); a single non-multi `sub f(byte $a)` accepts `f(1)`.

### 1.3 What mutsu does today (measured on `main` at 73ac287c, 2026-10-31)

- **A List literal already stores each variable's own cell, with its declared type on it.** `($a, $b)` compiles to
  `WrapVarRef` per element and `MakeArray`; `exec_make_array_op` (`src/vm/vm_data_ops.rs`) promotes the variable
  through `capture_var_cell_inner` and calls `register_container_cell_constraint_for_name`, which copies the declared
  type — `byte` included — onto the cell. Evidence: `my byte $t; my $l = ($t, 7); $l[0] = "x"` fails with
  `expected byte ... in assignment to $t`, `$l[0] = 300` writes `44` into `$t`, and `sub g(@a) { @a[0] = 9 };
  g(($x, 2))` writes `$x`. So the descriptor is already on the cell; the List is not the gap.
  (The `declared_untyped` comment in `src/value/mod.rs` describes cells made at *other* sites; the List promotion site
  registers the type.)
- **An element argument is dereferenced at the call site.** `f(@a[0])` compiles to `IndexArgRef`;
  `exec_index_arg_ref_op` hands the element's cell over only when the callee binds a container (`is rw`, `\x`, a bare
  block's topic) and otherwise runs the plain `Index` op, which returns the value. So the dispatcher never sees the
  cell for `n16(@a[0], @a[1])`.
- **The dispatcher reads a declared type only from a `VarRef`.** Three sites decide a native candidate, and each
  dereferences a `ContainerRef` argument without looking at its cell: the applicability filter
  `native_dispatch_arg_matches`, the ranking `unwrap_arg_for_dispatch`, and the multi-dispatch cache key
  `multi_arg_type_keys_impl` (which keys a `ContainerRef` by its contents only).
- `.head` / `.first` already return the cell to the dispatcher (a prototype that only changed the dispatcher sites made
  them pick `byte`); `$l[0]` with `$l` a scalar holding the List, `my \e = $l[0]` and `<->` do not.
- A literal `Int` is accepted by a multi whose only candidate takes `byte` (`n(1)` runs; rakudo refuses it at compile
  time). Unrelated to the symptom and left alone (§5).

### 1.4 Prototype (2026-10-31, not committed)

A 44-line prototype over four files — `exec_index_arg_ref_op` hands over a `List` slot's existing cell when that cell
carries a native `of`-type, and the three dispatcher sites take the type from a `ContainerRef` argument's cell — made
the §1.1 reduction print `256`, and turned the `($b, 5)[0]`, `@bound[0]`, `.head`, `.first` and `uint8` rows to
`byte` while `($b, 5)[1]`, `@arr[0]`, `[$b][0]`, `%h<a>` and `id($b)` stayed `Int`. 150 `t/` files matching
`multi|native|dispatch|alias` and the 20 whitelisted `S06-multi` / `S02-types/native` / `S09-typed-arrays/native`
roast files stayed green.

Two findings from it shape the decision:

- **Without the cache-key change the answer depends on call order**: the first call through a call site caches its
  winner under the contents' type `Int`, so `m(($b,5)[0]); m(($b,5)[1])` printed `byte byte` and the reverse order
  printed `Int Int`.
- The prototype read the type with `lookup_container_constraint`, which locks the cell's `constraint` mutex and
  clones a `String`. The cache-key site runs for every `ContainerRef` argument of every cached multi call (the
  Text::CSV hot path of #9494), so the real slice must not pay that.

## 2. Decision

1. **The dispatcher reads the native `of`-type of a container argument.** The three sites in §1.3 treat a
   `ContainerRef` argument whose cell carries a native `of-type` exactly like a `VarRef` of a variable declared with
   that type: `native_dispatch_arg_matches` accepts it for a native candidate, `unwrap_arg_for_dispatch` returns the
   type for ranking, and `multi_arg_type_keys_impl` keys it behind the existing `declared_type` marker. A
   `ContainerRef` without a native type keeps today's behaviour (a typed non-native cell is not keyed or ranked
   differently in this decision).
2. **The cell answers "am I native?" without a lock.** `ContainerCell` gains a native marker (an atomic, like
   `declared_untyped`), set where a native type is registered on the cell; the three sites test it first and read the
   type name only when it is set. Untyped and boxed-typed cells pay one relaxed atomic load.
3. **An element argument hands over its existing native-typed cell.** `exec_index_arg_ref_op` passes the slot's cell
   when the receiver is a `List` (also as held by a scalar, the `$l[0]` row) and the slot already holds a
   `ContainerRef` with the native marker. It never promotes a new cell, never touches `Array`/`Hash` receivers (their
   slots are fresh `Scalar`s, the `@arr[0]` row), and does not depend on the callee; a read-only parameter
   dereferences the cell at binding, as it already does for the cells `is rw` callees receive.
4. **Raw bindings of a List item keep the cell**: `my \e = $l[0]` and `for ... <-> $x`, matching §1.2. `->` and plain
   parameters keep unwrapping.
5. **Array copies keep losing it** (`my @arr = ($b, $b); @arr[0]` is `Int`), and a sub's non-`is raw` return keeps
   decontainerizing (`id($b)`).

The List literal itself needs no change (§1.3, first bullet).

## 3. Implementation slices

### Slice 1 — dispatch on a List item's native cell (closes #15309)

- `ContainerCell` native marker plus its setter, called from `register_container_constraint*` when the type's
  `native_family` is `Some` (`src/value/mod.rs`, ~20 lines).
- The three dispatcher sites (§2.1), each gated on the marker (~30 lines).
- `exec_index_arg_ref_op`: the List-slot hand-over (§2.3), including the scalar-held List (~30 lines).
- Test `t/routines/dispatch/multi-byte-list-item.t` (the path `scripts/migrate-t-layout.py --where` gives): every
  §1.2 row except `\e` and `<->`, the `uint8` row, both call orders of `($b,5)[0]` / `($b,5)[1]` through one call site
  (the cache), the §1.1 reduction, and a check that `id($l[0])` still sees a read-only `Int`.
- Verify Net::BGP's `t/00-conversions.t` (and the other files #15309 names) against `raku`; file each new failure the
  run exposes as its own issue.
- Size: one PR, ~100 lines of `src/` plus the test. Focus for the gate: `t/routines/dispatch`, `roast/S06-multi`,
  `roast/S02-types/native.t`, `roast/S09-typed-arrays`.

### Slice 2 — raw bindings of a List item (`\e`, `<->`)

- `my \e = $l[0]` and `for $l.list <-> $x` keep the item's cell (§2.4), so a later call dispatches on it.
- Test rows added to the slice 1 file. Size: small; one PR. Filed as its own issue when slice 1 lands, since
  Net::BGP does not need it.

## 4. Alternatives considered

- **A `byte`/`uint8` flag on the boxed `Int` value.** Rejected: rakudo's rows (`@arr[0]` is `Int`, `id($b)` is
  `Int`) show the property belongs to the container, not the value; a value-level tag would also survive copies that
  must drop it, and would need a `Value` variant on a hot path.
- **Accept any in-range `Int` for a `byte` candidate** (making the symptom disappear). Rejected: it changes dispatch
  for `multi (byte)` / `multi (Int)` pairs (`m(5)` would stop choosing `Int`) and does not match rakudo.
- **Pass every element argument as its container** (not only native-typed List cells). Rejected for now: it would
  promote `Array` slots and hand cells to callees that never look at them, for no dispatch difference; the native
  marker keeps the change to the rows rakudo actually distinguishes.
- **A process-wide "some native cell exists" flag** as the cost gate. Rejected in favour of the per-cell marker: one
  native variable anywhere would switch the lock-and-clone lookup on for every container argument in the program.
- **Special-case Net::BGP's recursion.** Rejected (AGENTS.md: no distribution-specific code).

## 5. Consequences and risks

- The multi-dispatch cache gains a key for native-typed container arguments; a cache key that misses it gives an
  order-dependent answer (§1.4), so the slice 1 test pins both orders through one call site.
- A native-typed List cell now reaches callees that are not `is rw`. Read-only binding already dereferences a
  `ContainerRef` (the prototype's `id($l[0])` still saw a read-only value, and builtins such as `sum` and `+` read
  it), but any pure builtin that matches on the raw argument view is a place to check if slice 1's focused runs show a
  regression.
- Out of scope, each its own issue: the literal `Int` accepted by a byte-only multi (§1.3, last bullet; #15852), and `.VAR`
  of a native variable (mutsu answers `Scalar` and `$b.VAR.of` gives `byte`, where rakudo answers `UIntLexRef` and has
  no `.of`) together with `($t,)[0].VAR.of` dying for `my Int $t` (rakudo: `Int`; #15853).
