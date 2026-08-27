# Multidim value adverbs (`:v`/`:k`/`:p`/`:kv`, plain and negated) get their correct shape for a hole or miss

`todo/tickets/multidim-value-adverb-hole-returns-nil-not-empty-list.md` reported a narrow-looking bug
found while landing `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`: `@a[i;j]:v` on a
multidim hole answered `Nil` instead of the empty list `()` that `raku` (and mutsu's own
single-dimension form, `t/typed-array-hole-adverbs.t`) already got right. Building the ticket's
requested "adverb x state" matrix against real `raku` turned up a much larger divergence in the same
three handlers (`builtin_multidim_subscript_adverb`, `multidim_subscript_adverb_multi`,
`builtin_multidim_subscript_adverb_dyn` in `src/runtime/builtins_multidim_ops.rs`), not just the
reported `Nil`-vs-`()` hole shape.

## What was actually wrong

The negated adverbs (`:!v`/`:!k`/`:!p`/`:!kv`) were wrong for a **filled** multidim slot, not just a
hole. `@a[0;0]:!k` on an assigned slot answered `Nil` where real Rakudo answers the raw value. Verified
directly against `raku`: unlike the single-dimension form (where `:!k` on a hole keeps reporting the
index, and `:!k` on a filled slot keeps reporting the index too -- each negated adverb keeps its own
key/pair/kv shape, only suppressing the *suppression*), real Rakudo's multidim `[;]` postcircumfix
collapses ALL FOUR negated adverbs to plain value access. `@a[i;j]:!k`, `:!p`, and `:!kv` all answer
the exact same raw value `:!v` would -- never the key or a pair -- both for a filled slot and for a
hole. This is a genuine Rakudo multidim quirk (confirmed with `raku`, not a mutsu assumption), and the
fix's 4-armed `match` in each handler collapsed to a single `"not-k" | "not-kv" | "not-p" | "not-v" =>
Ok(array_to_list(value))` arm to match it.

Fixing the plain (non-negated) hole-shape bug also surfaced a second wrinkle: a MISSING leaf is not
always `()`. Building the matrix against the vendored roast tests (not just bare `raku`) showed:

- `roast/S32-hash/multislice-6e.t` (under `use v6.e.PREVIEW`) pins `%hash{a;b;c}:k`/`:p`/`:v` on a
  missing compound key to `Nil`, not `()` -- explicit "gives Nil" assertions.
- `roast/S32-array/multislice-6e.t` pins the SAME `Nil` answer for `@array[i;j;k]:k`/`:p`/`:v` on an
  out-of-range coordinate into a plain nested/autoviv array.
- Plain `raku` (Array only, no `use` pragma) confirmed the ticket's own hole case (`my @a[2;2];
  @a[0;1]:v` -- an IN-BOUNDS, unassigned slot) is `()`.

The distinguishing signal turned out to be simple and was already available: an in-bounds Array hole
carries its own non-`Nil` hole marker (`ArrayData::hole_at`, e.g. `Package("Any")`) as its raw value,
so it answers `()`; everything else that fails to resolve (a genuine Hash miss, or an out-of-range/
non-numeric Array coordinate) is a bare `Value::NIL` with no marker of its own, and answers `Nil`. No
container-type inspection is needed -- `raw_value.is_nil()` alone (already computed by the existing
canonical hole predicate) picks the right answer. `:kv` is the one adverb that stays `()` in every
missing case (verified in both the roast tests and plain `raku`).

An early version of the fix instead tried to special-case "the parent container was Hash vs Array" via
a new recursive walk (`multidim_leaf_parent_is_array`); that broke `roast/S32-array/multislice-6e.t`
because an out-of-range Array coordinate needed the SAME `Nil` answer as a Hash miss, not the `()` an
Array hole gets -- the container's kind was the wrong signal; the value's own nil-ness was the right
one. That helper was removed before landing.

## What was already correct

The plain (non-negated) `:v`/`:k`/`:p`/`:kv` shape for an in-bounds Array hole, and `:kv`'s `()` shape
for a Hash miss, were already right before this fix (the previous code's uniform `Value::NIL` on `!exists`
happened to be wrong only for the Array-hole and filled-negated cases above -- the Hash-miss and `:kv`
cases were incidentally correct already).

## The fix

All three handlers (`builtin_multidim_subscript_adverb`'s single-coordinate branch,
`multidim_subscript_adverb_multi`'s Whatever/list-index per-leaf loop, and
`builtin_multidim_subscript_adverb_dyn`'s single-coordinate and multi-result branches) now share:

- `multidim_empty_list()`: the empty `()` `:kv` always reports for any miss, and what `:v`/`:k`/`:p`
  report for an in-bounds Array hole.
- `multidim_missing_result(&value)`: `Nil` if `value.is_nil()`, `()` otherwise -- what `:v`/`:k`/`:p`
  report for any OTHER kind of miss (Hash miss, out-of-range coordinate).
- A single collapsed `"not-k" | "not-kv" | "not-p" | "not-v" => Ok(array_to_list(value))` arm per
  handler, replacing the previous 4 separately-wrong arms.

`multidim_subscript_adverb_multi` (the Whatever/list-index path) has no raku oracle at all -- real
Rakudo throws `X::NYI` for any adverb combined with a multidim Whatever/list index (see the sibling
ADR-0049 news entry) -- so its negated-adverb collapse is a self-consistency choice, matching the
single-coordinate handler's shape rather than an independently-verified one.

## Known, deliberately-left divergence

A shaped array's out-of-range coordinate (`my @a[2;2]; @a[5;5]:v`) now answers `Nil` under mutsu,
matching the roast tests above, but plain (non-`v6.e.PREVIEW`) `raku` answers `()` for that specific
case. mutsu does not branch multidim-adverb behavior on the language-version pragma, so one rule has
to serve both, and roast (which is authoritative and gates CI) was chosen over the narrower plain-`raku`
answer. Filed as
`todo/tickets/multidim-oob-coordinate-nil-vs-empty-list-version-pragma.md` in case a future need for
per-version multidim semantics makes it worth revisiting.

## Verification

`t/typed-array-hole-adverbs.t` (the single-dimension form's existing pin) grew a multidim twin: 26 new
assertions covering `:v`/`:k`/`:p`/`:kv` and their negated forms on a filled slot, an in-bounds hole, an
out-of-range coordinate, and an autoviv (non-shaped) nested array -- all raku-comparable and verified to
pass identically under `raku` and `mutsu` (54 assertions total, up from 28). A targeted roast sweep of
every whitelisted `S09-*`, `S32-array/*`, and `S32-hash/*` file (59 files, 9532 tests, debug binary)
passes, including both `multislice-6e.t` files that the fix's intermediate version regressed and then
un-regressed. `cargo clippy -- -D warnings` and `cargo fmt` are clean, and `make test` passes with no
regressions.
