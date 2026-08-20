# Assigning a finite Range to an `@` variable silently truncates at 100 000 elements

Found 2026-08-20 while measuring the element-store path for
[ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) §5.2. It is
unrelated to that ADR, and it is **silent data loss**, not a laziness threshold.

## Repro

```
$ mutsu -e 'my @a = ^300_000; say @a.elems'          # 100000     raku: 300000
$ mutsu -e 'my @a = ^300_000; say @a[299_999]'       # (Any)      raku: 299999
$ mutsu -e 'my @a = 1..300_000; say @a.elems'        # 100001     raku: 300000
$ mutsu -e 'sub s(*@x){@x.elems}; say s(1..300_000)' # 100001     raku: 300000
$ mutsu -e 'my @c; @c[0..299_999] = 1..300_000; say @c.elems'   # 100001   raku: 300000
```

Everything that does *not* go through the `@`-assign path is correct, which is what makes the bug
easy to miss:

```
$ mutsu -e 'say (my @d = (^300_000).List).elems'   # 300000  (correct)
$ mutsu -e 'say (my @e = (^300_000).Array).elems'  # 300000  (correct)
$ mutsu -e 'say (1..300_000).elems'                # 300000  (correct)
$ mutsu -e 'my $n = 0; $n++ for ^300_000; say $n'  # 300000  (correct)
```

## Root cause

`MAX_ARRAY_EXPAND` — `src/runtime/utils/coerce_containers.rs:350`:

```rust
/// Maximum number of elements when expanding an infinite range into an Array.
/// TODO: Properly implement lazy arrays that reify elements on demand.
const MAX_ARRAY_EXPAND: i64 = 100_000;
```

The doc comment says **infinite** range, but `coerce_to_array` applies it unconditionally in all
four i64 `Range` arms — `:417`/`:423` (`Range`), `:429`/`:435` (`RangeExcl`), `:441`/`:447`
(`RangeExclStart`), `:453`/`:459` (`RangeExclBoth`) — in the *finite* `else` branch as well as the
`b == i64::MAX` one:

```rust
let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
```

That reproduces the numbers exactly: `^300_000` → `end = min(300000, 0+100000) = 100000`;
`1..300_000` → `end = min(300000, 1+100000) = 100001`.

The finite branch builds `Value::real_array(...)` — an ordinary `ArrayKind::Array` with no
`sequence_spec`, no `LazyList`, and no retained source. **There is nothing left to reify from**,
which is why `@a[299_999]` returns `(Any)` rather than materialising on demand. Only the
`b == i64::MAX` branch reaches the lazy machinery (`ArrayKind::Lazy` /
`infinite_int_range_to_lazy_array` / `force_lazy_list_vm_n`), where the cap is the intended
behaviour.

Two sibling constants have the identical unconditional shape and the same bug (confirmed by the
`slurpy` and `slice-assign` repros above):

- `MAX_ASSIGN_SLICE_EXPAND = 100_000` — `src/vm/vm_var_assign_coerce.rs:503` (`assignment_rhs_values`)
- `MAX_SLURPY_RANGE_EXPAND = 100_000` — `src/runtime/types/signature.rs:130` (slurpy `*@a` binding)

For contrast, `value_to_list` (`src/runtime/utils/list.rs:97-113`) uses a *different* constant
`MAX_RANGE_EXPAND = 1_000_000` (`src/runtime/utils.rs:10`), which is why `.List`/`.Array` are
correct at 300k — and would truncate the same way above 1M.

## Fix shape

Gate each cap on the infinite branch only; the finite branch already has a real bound in `b` and
should expand fully. Do all three constants in one pass — they are the same defect — and decide
deliberately whether the three remaining infinite-range caps should share one constant with
`MAX_RANGE_EXPAND` rather than being four independent numbers.

## Why it is a ticket, not a deep finding

The change is local and the correct behaviour is unambiguous. The only judgement call is whether a
genuinely unbounded finite range (`my @a = 1..2**40`) should OOM or keep a cap; raku itself will
happily try, so matching raku is defensible, but a very large finite range is the one case worth a
conscious decision before landing.

## Pin

`t/finite-range-assign-no-truncation.t` — the five diverging repros above plus the four correct
controls, and one case per sibling constant. There is currently **no** test anywhere in `t/`
covering a >100k finite range assignment.

## Prior art

`docs/lazy-arrays.md` (~line 226) lists "the capping points (all use the constant `100_000`)" but
scopes them to `b == i64::MAX`; it does not record that the finite branch is capped too.
`todo/deep/lazy-array-element-assign-reifies-100k.md` is a *different* 100k bug
(`reify_lazy_array_slot`, `src/vm/vm_helpers_lazy.rs:500`). This one appears unrecorded in `todo/`,
`docs/`, `TODO_roast/`, `news/`, and `t/`.
