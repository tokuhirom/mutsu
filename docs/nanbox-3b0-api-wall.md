# 3b-0: the `Value` API wall (NaN-boxing enabler)

Status: In progress (slice a landed)
Related: [ADR-0001](adr/0001-gc-strategy-and-phasing.md) §3-6,
[gc-post-3a-roadmap.md](gc-post-3a-roadmap.md) §3.3, [PLAN.md](../PLAN.md) §5 Lever 2

## 1. Goal

Layer 3b replaces the 48-byte `Value` enum with an 8-byte NaN-boxed
representation (roadmap §3.2). That flip is only feasible if **no code outside
`src/value/` mentions a `Value` enum variant directly** — every direct
`Value::Int(...)` pattern or constructor breaks the moment `Value` stops being
a Rust enum. Slice 3b-0 builds that wall *while `Value` is still the enum*, so
the representation switch (3b-1) only has to change accessor internals.

The wall has three faces:

1. **Constructors** — `Value::int(3)` instead of the variant expression
   `Value::Int(3)`. (A convention that already exists for the boxed variants:
   `Value::str()`, `Value::capture()`, `Value::bigrat()`, ...)
2. **Views** — `match v.view() { ValueView::Int(n) => ... }` instead of
   `match v { Value::Int(n) => ... }`. `ValueView<'a>` is an exhaustive
   borrowed mirror of the enum, so exhaustive matches stay exhaustive.
3. **Accessors / mutators** — `v.as_int()`, `v.as_str_arc()`, closure-based
   `with_*_mut` entry points for in-place payload mutation.

Everything **inside `src/value/`** may keep touching variants directly: that
directory *is* the representation module and gets rewritten wholesale at 3b-1.
The one historical exception outside it (`runtime/value_iterator.rs` has an
`impl` block on a Value-adjacent type) is migrated like any other caller.

## 2. Why these shapes survive NaN-boxing

Post-3b-1 the payload of a `Value` is one of:

- **inline scalar** — small `Int`, `Bool`, `Nil`, `Num` (raw f64), `Symbol`
  payloads (`Package`, ...). View can return these **by value** (they are
  decoded copies). `as_int() -> Option<i64>` works unchanged.
- **direct-packed pointer** — single-pointer variants (`Str(Arc<String>)`,
  `Hash(Gc<HashData>)`, `Seq(Arc<Vec<Value>>)`, `Sub(Gc<SubData>)`, ...)
  store the raw pointer bits in the box payload. A `&'a T` *deref target*
  (`&str`, `&HashData`) is always reconstructible. A `&'a Arc<T>` /
  `&'a Gc<T>` (reference to the smart-pointer struct itself) is
  reconstructible **only** under a pointer-favored encoding (pointers stored
  as raw low-48-bit values, doubles offset-encoded), where `&self.0` can be
  transmuted to `&Arc<T>`. If 3b-1 instead picks a NaN-favored encoding
  (tag bits set inside the same u64), these view fields become by-value
  guard types (`ArcRef<'a, T>` / `GcRef<'a, T>`: a `ManuallyDrop`
  reconstruction that `Deref`s to `Arc<T>`/`Gc<T>` without touching the
  refcount). **Migration rule that keeps both exits open:** call sites must
  use smart-pointer bindings deref-compatibly — `s.clone()`, `s.is_empty()`,
  `&*s`, `Arc::ptr_eq(&a, &b)` are all fine; storing the `&Arc` itself in a
  struct or comparing its address is not.
- **heap-boxed struct** — multi-field variants (`Instance`, `GenericRange`,
  `Junction`, `Pair`, `Capture`, `Version`, `Mixin`, `Proxy`, rare/large
  variants like `BigRat`, `RegexWithAdverbs`, `LazyIoLines`,
  `HashEntryRef`, ...) move behind one heap allocation holding a plain
  payload struct. References **into** that struct (`&'a Arc<Value>`,
  `&'a Vec<Value>`, `&'a EnumValue`, ...) are trivially reconstructible, so
  the view can keep field-by-field reference payloads for these.

Mutation is the one shape that cannot survive as a plain `&mut` projection:
post-box there is no `&mut Gc<ArrayData>` inside the packed u64 to hand out.
Closure-based mutators (`with_array_mut(|gc, kind| ...)`) survive any
representation: the impl unpacks to a local, runs the closure, repacks.
While `Value` is still the enum they compile to a plain match (zero cost).

## 3. Call-site migration recipe (mechanical)

| Old shape | New shape |
|---|---|
| `Value::Int(x)` (expression) | `Value::int(x)` |
| `Value::Nil` (expression) | `Value::NIL` |
| `Value::Bool(b)` / `Value::Bool(true)` | `Value::truth(b)` / `Value::TRUE` |
| `match v { Value::X(p) => ... }` (v: `&Value`) | `match v.view() { ValueView::X(p) => ... }` |
| `if let Value::X(p) = &v` | `if let ValueView::X(p) = v.view()` |
| `matches!(v, Value::X(..))` | `matches!(v.view(), ValueView::X(..))` |
| `Some(Value::X(p))` nested in `Option<&Value>` match | `.map(Value::view)` then `Some(ValueView::X(p))` |
| `match v { Value::Str(s) => s, ... }` (move out) | `into_*` accessor, or view + clone (an extra refcount bump, not a deep copy) |
| `match &mut v { Value::Array(gc, k) => ... }` | `v.with_array_mut(\|gc, k\| ...)` (add per-shape mutators as encountered) |
| single-variant probe | `as_int()` / `as_num()` / `as_bool()` / `as_str()` / `is_nil()` ... |

Bindings that used to be `&String` / `&Box<Value>` become `&str`-compatible
`&String` and `&Value` in the view (the `Box` never appears). Sites that did
`**boxed` now write `*bound`; this is the only non-sed-able adjustment seen
so far and it is caught by the compiler.

Guard-rails while migrating:

- Behavior must be byte-identical; each slice gates on `make test` (and CI
  gates the full roast).
- Matches on **owned** `Value` (`match self`) outside `src/value/` should be
  rewritten against `.view()` + explicit clones of the payloads actually
  used, or moved behind an `into_*` accessor — never left direct.
- Do not "improve" logic while migrating. Pure transliteration only.

## 4. Zero-cost claim

`view()` is an `#[inline]` single match that copies scalar payloads and
re-borrows pointer payloads; LLVM collapses `match v.view()` to the same jump
table / discriminant tests as `match v`. The hot arithmetic dispatch
(`vm_arith_ops.rs`) is migrated in slice a precisely to keep this observable:
any regression shows up in `benchmarks/` (fib / int-arith) immediately.

## 5. Slice plan and ratchet

- **a (this slice)**: design + `src/value/view.rs` (full `ValueView` mirror,
  `view()`, core scalar accessors, missing scalar constructors) + exemplar
  migrations proving the recipe on real shapes (`vm/vm_arith_ops.rs`:
  hot-path if-let chains; `vm/vm_loop_writeback.rs`: tuple matches, nested
  `Option` patterns, `into_array` move-out; `vm/vm_native_map.rs`: let-else,
  `matches!`, owned-match fallthrough) + ratchet script.
- **b..**: directory-sized mechanical slices, roughly in this order (biggest
  wins / hottest first): `src/vm/`, `src/builtins/`, `src/runtime/`,
  `src/compiler/` + stragglers. Slices are independent and parallelizable;
  each updates the ratchet baseline downward.
- **final**: baseline hits 0 outside `src/value/`; add the variant-privacy
  seal (rename variants or make the enum private) so no new direct use can
  land; then 3b-1 can start.

**Ratchet**: `scripts/check-value-wall.sh` counts direct variant mentions
outside `src/value/` and fails if the count *rises* above
`scripts/value-wall-baseline.txt`. Run as part of `make test`. When your PR
lowers the count, lower the baseline in the same PR (the script prints the
new number; `scripts/check-value-wall.sh --update` rewrites it).

## 6. Open questions deferred to 3b-1

- Pointer-favored vs NaN-favored encoding (decides `&'a Arc<T>` vs guard
  types in the view — see §2). Both are compatible with the wall.
- Small-int width (48-bit inline vs 32-bit inline vs `Gc<i64>` box) — decided
  by int-arith bench at flip time (roadmap §3.2).
- Whether `ArrayKind` / the Set/Bag/Mix mutability bool live in spare tag
  bits or inside the pointee.
