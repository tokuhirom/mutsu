# Assigning a finite Range of more than 100,000 elements no longer truncates

Assigning a **finite** `Range` of more than 100,000 elements to an `@` variable — or binding one to
a slurpy `*@a` parameter, or using one as the RHS or index set of a slice assignment — used to
silently truncate to 100,000 elements. `my @a = ^300_000; say @a.elems` printed `100000` instead of
`300000`, and `@a[299_999]` read back `(Any)` instead of `299999`. This was silent data loss, not a
laziness threshold: the finite branch built a plain `ArrayKind::Array` with no `sequence_spec` or
retained source, so nothing was left to reify from once truncated.

## Root cause

`coerce_to_array` (`src/runtime/utils/coerce_containers.rs`), `assignment_rhs_values` and
`slice_indices_from_index` (`src/vm/vm_var_assign_coerce.rs` / `src/vm/vm_var_assign_typed.rs`), and
`flatten_into_slurpy` (`src/runtime/types/signature.rs`) each applied a 100,000-element cap
unconditionally to every i64 `Range`/`RangeExcl`/`RangeExclStart`/`RangeExclBoth` value, in the
*finite* branch as well as the intended *infinite* one (`b == i64::MAX`, e.g. `^Inf`, `1..*`). Only
the infinite branch was supposed to hit the cap — it exists so that binding a genuinely infinite
range into a `Lazy`-kind array doesn't loop forever, not to bound ordinary array assignment.
`signature.rs`'s `GenericRange` fast path (both endpoints already typed `Int`) had the identical
unconditional cap even though an `Int` endpoint can never represent infinity, so it was wrong in
every case it could reach.

## Fix

Each cap now only applies when the range's upper bound is the infinite sentinel (`b == i64::MAX`).
A finite range — which always has a real, known bound — expands to it in full, matching real `raku`
(`raku -e 'my @a = 1..2**40; say @a.elems'` genuinely attempts to materialize the range rather than
refusing with a sanity limit; it was still running, unbounded, after 25 seconds under `timeout`).
mutsu now matches that: there is no mutsu-only safety cap on a finite range's size, only real memory
is the limit, since inventing an artificial cap here would itself be a compatibility divergence.

The three previously-independent 100,000 literals (`MAX_ARRAY_EXPAND`, `MAX_ASSIGN_SLICE_EXPAND`,
`MAX_SLURPY_RANGE_EXPAND`) were unified into a single shared constant,
`crate::runtime::utils::MAX_LAZY_RANGE_PREFIX` — they all capped the exact same case (materializing
the initial prefix of a genuinely infinite `Range`) and had drifted into separate numbers purely by
accident of file layout. `MAX_RANGE_EXPAND` (1,000,000, used by `value_to_list` for a full one-shot
`.List`/`.Array` coercion) was deliberately left separate, since it governs a different, non-lazy
materialization path with its own established, larger allowance.

`docs/lazy-arrays.md` already correctly documented the cap as scoped to `b == i64::MAX` — it was the
*code* that diverged from its own doc. The doc's capping-point list and constant names were updated
to match the unified constant and the current file locations.

## Pin

`t/finite-range-assign-no-truncation.t` — the five diverging repros, four correct controls, an
infinite-range laziness check per code path, and a >100k case for each of the two sibling
constants (slurpy binding, slice assignment).
