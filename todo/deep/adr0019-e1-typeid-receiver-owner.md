# ADR-0019 E1 design: stable TypeId and receiver-owner resolution

Design pass for Phase E box E1 (see the ADR's Phase E section). E1 is the foundation of the
whole phase: the reverted handler-ID attempt (b252837e7, reverted f1485d136, 2026-08-04) failed
not because handler IDs are wrong but because it consulted the registry with owners produced by
`value_type_name()`, which is not a dispatch owner. Everything later in Phase E (E2 rows, E4
resolver, E3 cache keys) keys on the owner E1 defines, so E1 must land first and land shadow-first.
No code has landed for this box yet.

## Problem statement

The "who owns this method for this receiver" decision is scattered and inconsistent:

1. **`value_type_name()`** (`src/runtime/utils/type_misc.rs:3`, returns `&'static str`) answers
   `"Any"` for every user-class `Instance` (type_misc.rs:96), `"Package"` for every type object
   (type_misc.rs:93), and bakes representation aliases into itself (`Hash` with
   `declared_type == Some("Map")` → `"Map"` at :24; `LazyList` → `"Seq"`/`"Array"` by
   `is_from_gather()` at :22; `Array(_, kind)` → `"Array"`/`"List"` by `kind.is_real_array()`
   at :18).
2. **Four near-duplicate naming paths** exist: `value_type_name`, `what_type_name`
   (`src/value/types.rs:4` — resolves Instance/Package names, no Map alias, adds the
   `+{Role}` mixin suffix), `Value::isa_check`'s inline third copy of the alias table
   (`src/value/types_isa.rs:6`), and `dispatch_caret_name` (`.^name`,
   `src/runtime/methods_introspect.rs:615`).
3. **Four independent builtin MRO tables** that disagree with each other:
   - `builtin_type_parents` (`src/builtins/builtin_type_methods.rs:845-862`): chains up to but
     not including Any/Mu; has `Hash→[Hash,Map,Cool]`.
   - `Registry::builtin_mro_table` (`src/runtime/registry.rs:573-615`): a disjoint set (Match,
     Capture, IO::Spec::*, Distribution::*, CompUnit::*).
   - `Interpreter::builtin_type_mro_chain` (`src/runtime/methods_call_helpers.rs:454-475`):
     includes Any/Mu, but `Hash→[Hash,Cool,Any,Mu]` (no Map), plus Set/Bag/Mix/Regex/Sub/Junction
     rows absent from the first table.
   - `builtin_type_distance`'s inline table (`src/runtime/resolution_method.rs:473-503`): a
     fourth spelling that also interleaves roles (`Int→[Int,Numeric,Real,Cool,Any,Mu]`,
     `Sub→[Sub,Routine,Block,Code,Callable,Any,Mu]`), plus the Buf/Blob family table in
     `dispatch_candidates.rs:672-700`.
4. **~27 owner-decision sites** feed dispatch or the `(owner, method)` registry lookup, each
   re-deriving the owner by hand. The dispatch-critical ones: `call_method_with_values`'s
   augment gate (`methods_call_dispatch.rs:3646`), `dispatch_instance_and_fallback`'s
   dispatch-class pick with its own hand-rolled `["Routine","Block","Code","Callable"]` walk
   (`methods_instance_ops.rs:1675-1696`), the `.^add_fallback` MRO extension
   (`methods_instance_ops.rs:2304`), qualified dispatch (`methods_qualified.rs:897`),
   `type_matches_value` (`types/type_matching.rs:1463`), and the multi-cache arg keys
   (`vm_call_method_compiled_cache.rs:52`). Plus the 13 literal
   `_ => value_type_name(&args[0]).to_string()` fallback arms in `methods_classhow_dispatch.rs`
   (lines 174, 201, 258, 285, 396, 438, 561, 584, 977, 1050, 1207, 1228, 1236) and the same
   3-arm shape repeated 8 more times across `methods_classhow_mro.rs`,
   `methods_classhow_parents.rs`, `methods_classhow_builtin_methods.rs`,
   `methods_classhow_lookup.rs`, `methods_classhow_method_obj.rs`.
5. **No integer type identity exists.** `grep -rn "TypeId" src` → 0 hits. `Symbol(u32)`
   (`src/symbol.rs:14`, append-only global intern table, `&'static str` resolution) is the only
   interned identity. `MethodEntryKey { owner: Symbol, name: Symbol }` (`registry.rs:44-48`)
   already uses it, but `Registry.classes`/`roles`/etc. are `String`-keyed, and hot-path
   comparisons like `sym == "Array"` go through a table lookup + byte compare
   (`symbol.rs:29-39` — NOT O(1)).

## Facts the design rests on (survey results, 2026-08-09)

- The registry write side is already Symbol-keyed and canonical-in-shape:
  `sync_user_method_entries` interns the passed `class_name` verbatim (`registry.rs:321`), and
  builtin rows are keyed by `canonical_builtin_owner` (`builtin_type_methods.rs:766-788`), which
  folds `Rat|FatRat→Rat`, `Sub|Method|Block|Routine|Code→Code`, buf/blob classes→`Blob`.
- User-class MRO is already computed and cached as `Arc<[Symbol]>` (`ClassDef::mro`,
  `decl_types.rs:31`; `Registry::class_mro` `registry.rs:621-651`, `class_mro_readonly`
  :660-686). `compute_class_mro` (`registry.rs:439-570`) is string-based C3 with special cases
  (role parents re-ordered, `Any`/`Cool` seeds, parametric parents).
- Builtin subclasses (`class Foo is Array`) have **no** modeled Array method table; dispatch
  reaches native Array methods by delegating to the `__mutsu_array_storage` backing attribute at
  ~14 scattered sites, and `nextsame` needs the `native_array_storage_next_candidate` synthesized
  fallback (`builtins_dispatch_next.rs:263-310`).
- Mixins record roles as map keys `__mutsu_role__{Name}` inside a `HashMap`;
  `dispatch_mixin_method_call` (`methods_mixin_dispatch.rs:7`) walks those keys **in HashMap
  iteration order** — the multi-role precedence is not deterministically ordered today.
- Role puns materialize a real `ClassDef` under the role's name with
  `mro: [role, Any, Mu]` (`registration_class_augment.rs:1116-1131`), so puns resolve through
  the normal user-class path once punned.
- Enums: `value_type_name` says `"Int"` (`type_misc.rs` Enum arm), while `.^can` special-cases
  `Enum → enum_type` (`methods_classhow_method_obj.rs:295`) — two different owners for the same
  receiver depending on entry point.
- Thread forking COW-clones the registry (`runtime_thread.rs:439-446`); any *dense* id space
  owned by the registry would fork with it, while `Symbol` is process-global and append-only.

## Design decisions

**1. `TypeId` is a newtype over `Symbol`, not a dense index.**

```rust
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct TypeId(Symbol);
```

The newtype's value is the *invariant*, not the representation: a `TypeId` may only be produced
by the E1 classifier or the E1 catalog, so holding one proves the name went through owner
canonicalization (aliases folded, Instance/Package resolved to the class symbol). Dense ids are
rejected because (a) the registry is COW-forked per thread, so a registry-owned id space would
diverge across forks while Symbols cannot; (b) `MethodEntryKey.owner` is already a Symbol, so no
key migration is needed; (c) `Registry.classes` et al. are `String`-keyed and their migration is
Phase-F cleanup, which dense ids would drag into E1. Hot-path string comparisons are eliminated
by a lazily-initialized well-known-symbol struct (one-time interning):

```rust
pub(crate) struct WellKnownTypes { pub any: TypeId, pub mu: TypeId, pub cool: TypeId,
    pub array: TypeId, pub list: TypeId, pub hash: TypeId, pub map: TypeId, /* ... */ }
```

so `chain.contains(&wk.array)` is a u32 compare, never `sym == "Array"`.

**2. One static builtin type catalog replaces the four MRO tables.**

A single `builtin_type_info()` static table (living beside the method catalog in
`builtin_type_methods.rs`, since F3 will fuse them) with one row per builtin type:

```rust
pub(crate) struct BuiltinTypeInfo {
    pub name: &'static str,
    pub mro: &'static [&'static str],    // full linear chain incl. Any, Mu
    pub roles: &'static [&'static str],  // Positional/Associative/Callable/Numeric/Real/Stringy...
    pub dispatch_owner: &'static str,    // canonical_builtin_owner successor ("" = self)
}
```

The linear `mro` serves dispatch walks; the `roles` list serves type-matching/distance (the
current `builtin_type_distance` and `dispatch_candidates` tables interleave roles into the
chain — the catalog keeps them separate because MRO order and role membership are different
facts, and `.^mro` must not report roles). The four existing tables become readers of this one
(or are deleted where E1b's cutover replaces their call sites). `canonical_builtin_owner` folds
into `dispatch_owner`.

Authority for the rows is **raku, not the union of the current tables**: V1 below adjudicates
every divergence against `raku -e 'say T.^mro'` / `T.^roles` output, recorded in the test that
pins the catalog. Divergences from *current mutsu behavior* discovered this way do NOT get
silently fixed in E1a (zero-behavior-change rule); they are listed in the E1a PR as an
accepted-mismatch ledger and flipped deliberately in E1b (or filed as tickets if they turn out
to be visible bug fixes with their own blast radius).

**3. One receiver classifier: `receiver_dispatch_class(&Value) -> ReceiverClass`.**

```rust
pub(crate) struct ReceiverClass {
    pub type_id: TypeId,          // canonical dispatch owner of the receiver itself
    pub definedness: Definedness, // Concrete | TypeObject  (for :D/:U invocant filtering, E4)
    pub exec: ReceiverExec,       // Direct | ArrayStorageDelegate | MixinLayered | ...
}
pub(crate) enum Definedness { Concrete, TypeObject }
```

plus `dispatch_mro(&Value) -> SmallVec<[TypeId; 8]>` returning the full ordered owner chain:

- `Instance { class_name }` → `class_name`'s registry MRO (already `Arc<[Symbol]>`), Concrete.
- `Package(name)` → the same chain as an instance of `name`, TypeObject. (This is the fix for
  "type objects appeared as Package": a type object's dispatch chain is its type's chain; only
  definedness differs.)
- Builtin concrete values → catalog chain (aliases resolved here and only here: `Map`-declared
  Hash starts at `Map`, gather-LazyList at `Seq`, `ArrayKind` non-real arrays at `List`,
  Set/Bag/Mix mutability variants, `BigRat`→`FatRat`, allomorph mixins→`IntStr` etc.).
- `Mixin(inner, mixins)` → role TypeIds first, then the inner chain; `exec: MixinLayered`.
  Role order within one mixin layer must be made deterministic (V2).
- Enum values → `[enum_type, Int-chain...]` (V3).
- User class `is Array` → the registry MRO already contains `Array`, and the catalog supplies
  `Array`'s tail, so the chain is `[Foo, Array, List, Cool, Any, Mu]`; `exec:
  ArrayStorageDelegate` tells the execution layer (E4/E5) that native list handlers run against
  the `__mutsu_array_storage` attribute. E1 itself does not touch the 14 delegation sites; it
  only makes the chain *say* what they currently hand-code.
- `Scalar`/`ContainerRef`/`VarRef`/`LazyThunk` → recurse to the held value, as
  `value_type_name` does today.

**4. E1a shadow mode: compare the classifier against today's decisions at four choke points.**

Zero behavior change. At each site, compute the new owner beside the old and bump
`MUTSU_VM_STATS`-gated counters `owner_shadow_checks` / `owner_shadow_mismatches` (plus a
per-site breakdown counter so a mismatch names its site). The four sites, chosen because they
are the dispatch-path decisions E5-E7 will route through the resolver:

1. `call_method_with_values`'s `value_type_name` augment gate
   (`methods_call_dispatch.rs:3646`) — compare `chain[0]`'s name.
2. `dispatch_instance_and_fallback`'s dispatch-class pick (`methods_instance_ops.rs:1675`) —
   compare the picked class.
3. `try_compiled_method_or_interpret_inner`'s Instance/Package owner
   (`vm_call_method_compiled_interpret.rs`, the block feeding `resolve_method_cached`).
4. `multi_arg_type_keys` (`vm_call_method_compiled_cache.rs:52`) — compare the per-arg key
   symbol.

The shadow target is **"the new classifier reproduces the site's current decision"**, not "the
new classifier is right". Where the classifier is deliberately different (type objects,
subclasses, aliases — the failure modes the box exists for), the mismatch is *expected*; the
E1a exit criterion is therefore not a raw zero but: every mismatch bucket on a full `t/` +
whitelisted-roast sweep is either zero or matched to a line in the accepted-mismatch ledger.
An unexplained bucket blocks E1b.

**5. E1b cutover: dispatch sites first, MOP fallbacks as their own PR.**

E1b makes the classifier authoritative at the four sites above plus the remaining
dispatch-path owner scans (`methods_instance_ops.rs:2264/2304/2494/2536`,
`methods_qualified.rs:897`, `type_matching.rs:1463`, `methods_call_helpers.rs:292`), deleting
the per-site string logic. The 13+8 MOP fallback arms collapse into one shared helper
`mop_receiver_owner(&Value) -> String` (the existing 3-arm Package/Instance/fallback shape,
now backed by the classifier) as **E1c**, a separate mostly-mechanical PR — the ADR already
allows "the MOP fallback sites may follow as their own PR if the diff warrants it", and the
survey says it will (21 sites across 6 files).

**Out of scope for E1** (state this in the PR description to keep review honest): display
naming (`.^name`, `what_type_name`'s `+{Role}` suffix, `user_facing_type_name` mangling) is a
*presentation* concern and stays as-is; `isa_check`'s table unification is a candidate follow-up
ticket once the catalog exists; fixing the mixin-order nondeterminism (V2) lands here only if
V2 shows raku semantics require it, otherwise it is filed as a ticket.

## Slice plan

- **E1a — catalog + classifier + shadow counters.** The `TypeId` newtype, `WellKnownTypes`,
  `BuiltinTypeInfo` static catalog with the raku-adjudicated rows and a unit test pinning them,
  `receiver_dispatch_class`/`dispatch_mro`, the four shadow probes, the stats counters, and the
  accepted-mismatch ledger in the PR description. Zero behavior change; validate with full `t/`,
  the stats sweep, and `cargo test`.
- **E1b — dispatch-site cutover.** Classifier becomes authoritative at the enumerated dispatch
  sites; per-site string scans deleted; the four MRO tables' dispatch-path readers move to the
  catalog (introspection readers like `classhow_mro_names`' Grammar special case may keep their
  own logic until E7/F2 if their output is observably different — decide per reader from the
  ledger). Because this changes how names and types resolve, run a local `make roast` before
  the PR (per the working agreement), not just `make test`.
- **E1c — MOP fallback consolidation.** `mop_receiver_owner` helper; mechanical replacement of
  the 21 fallback arms; no semantic change beyond the E1b-established owner rules.

## Verification items (resolve during E1a)

- **V1 — adjudicate the four-table divergence against raku.** For every type named in any of
  the four tables, capture `raku -e '<T>.^mro.say; <T>.^roles.say'` and record the catalog row
  from that output. Known divergences to adjudicate explicitly: `Hash` (Map in the chain or
  not), `Sub` (Callable placement), `Bool` (raku: `Bool is Int`), `Seq`, `Junction` (raku:
  `Junction → Mu`, skipping Any), Buf/Blob parameterized names, `Nil` (`resolution_method.rs`
  hand-codes `[Nil, Cool, Any, Mu]`).
- **V2 — mixin role order.** `my $x = 0 but A but B` where both roles define `m`: check raku's
  winner, then check whether mutsu's current HashMap-order walk is deterministic for the same
  program across runs. If raku requires later-wins and mutsu is nondeterministic, that is a
  pre-existing bug to file (and the classifier's chain ordering fixes it in E1b); the
  classifier must at minimum be deterministic.
- **V3 — enum receivers.** raku: `enum E <a b>; say a.^mro` — expect the enum type before Int.
  Confirm which owner today's dispatch actually uses at each entry (`.^can` uses enum_type,
  `value_type_name` says Int) and pick the chain accordingly.
- **V4 — allomorphs.** `<1/3>`, `IntStr` etc.: confirm the catalog rows and that the mixin
  shortcut in `value_type_name:116-130` is reproduced by the classifier.
- **V5 — parameterized Buf/Blob names.** `is_buf_or_blob_class` accepts dynamic names
  (`buf8`, `Buf[uint8]`); the classifier must resolve them to catalog rows without allocating
  per call (intern once per distinct name is acceptable — Symbol interning already memoizes).

## Risk notes

The classifier itself is additive and shadow-verified, so E1a is low-risk by construction. The
risk concentrates in E1b: an owner change at a dispatch site changes *which* method table is
consulted first, and the blast radius is every method call. Mitigations: the E1a ledger must be
complete before E1b starts (no "we'll see in CI"); E1b runs local `make roast`; and E1b's diff
should be reviewable site-by-site — if it grows past ~10 sites, split it. Perf: the classifier
must not allocate on the hot path (SmallVec chain, `Arc<[Symbol]>` reuse for instances); add the
chain construction to an existing bench sanity check rather than a new counter gate.
