# Z/Z=> silently truncated large finite operands to 1000

The `Z`/`Z=>` meta-operator's binary and n-ary arms in `exec_meta_op`/
`exec_meta_op_nary` unconditionally clamped the zipped result length to
`MAX_ZIP_EXPAND` (1000) — a cap meant only to bound genuinely infinite
`Range`s and `LazyList`s so `1..* Z** 1..*` doesn't hang. Two plain finite
`Array`s (or `Range`s) longer than 1000 elements got silently truncated to
1000 pairs instead of zipped in full, with no error.

`ZipIter` compounded this for `Range`: `from_value` capped a `Range`'s
element count at `MAX_ZIP_EXPAND` regardless of whether the range was
actually finite, and `is_lazy()` then inferred laziness from `count >=
MAX_ZIP_EXPAND` — so a perfectly finite `1..2000 Z 1..2000` reported itself
as lazy purely because its count happened to be clamped to exactly 1000,
producing a `LazyList` value whose `.elems` raises "Cannot .elems a lazy
list".

Found via the ecosystem roulette on `DSL::Entity::Metadata` (locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)): its dependency
`DSL::Shared::Entity::ResourceAccessish` builds a name-to-entity-ID `Hash`
from a 6428-line resource file via `%nameRules.keys.map(*.lc) Z=>
%nameRules.values`, silently losing every entry past the 1000th pair and
making `.known-name('Dataset', ...)` fail for names appearing later in the
file (e.g. `t/Metadata-names-parsing.t`'s "u n human rights swedish").

Fixed generally, not for this distribution specifically:

- `ZipIter` now tracks whether a `Range` is genuinely infinite (its
  endpoint `== i64::MAX`, matching the check the `X` meta-op already uses)
  instead of inferring it from a clamped count. A finite range's true
  count is no longer capped.
- `zip_iter_from_value` builds a genuinely infinite range's iterator
  bounded by `needed` — typically the other operand's real length — via
  the new `ZipIter::from_infinite_range`, instead of always defaulting to
  the coarse fixed cap.
- The `Z`/`Z=>` binary and n-ary arms only apply the `MAX_ZIP_EXPAND`
  safety clamp when every operand is genuinely unbounded (`all_lazy`); a
  zip with at least one finite side now returns its true length. The clamp
  still applies to two genuinely infinite operands, and to a
  trailing-`*`-extended list whose `.len()` is `usize::MAX`.
- `is_zip_unbounded` treats an infinite `Range` the same as a `LazyList`
  operand when deciding how much of each side to pull, matching the `X`
  meta-op's existing `Range`-infinity check.

Pinned by `t/collections/transform/zip-large-operands.t`. `DSL::Entity::Metadata`
moves from `partial` (2/3 baseline files) to `green` (3/3, 24/24 assertions).
