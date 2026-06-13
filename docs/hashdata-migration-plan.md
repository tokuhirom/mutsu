# `Value::Hash` → `Arc<HashData>` migration (stable-container-ID, PLAN Q2 本筋)

> Goal: kill the `Arc::as_ptr`-keyed side tables (`hash_type_metadata`,
> `register_hash_original_keys`, …) — the root of the intermittent typed-hash /
> object-hash flaky — by carrying the metadata IN the hash value, so it survives
> copy-on-write and pointer reuse. Unblocks object-hash (`%{Mu}`, classify.t,
> objecthash.t) and removes a class of flaky.

## Status (2026-06-12)

**Stage 1a — HashData wrapper (DONE, on branch `refactor/hashdata-wrapper`,
pushed, NOT merged).** `Value::Hash(Arc<HashMap>)` → `Value::Hash(Arc<HashData>)`.

```rust
pub struct HashData {
    pub map: HashMap<String, Value>,
    pub value_type: Option<String>,    // element type (my Int %h)
    pub key_type: Option<String>,      // object-hash key type (my %h{Mu})
    pub original_keys: Option<HashMap<String, Value>>, // WHICH -> key object
}
// Deref/DerefMut -> map (reads unchanged); PartialEq compares map only.
```

- 120 compile errors fixed across 31 files. `Value::hash`/`hash_arc` take
  `impl Into<HashData>` (accept HashMap OR HashData, preserving meta on rebuild).
  Deref absorbed ~398 read sites; only structural mutation/rebuild sites changed.
- Builds, clippy-clean. The `value_type`/`key_type`/`original_keys` fields are
  **declared but not yet populated/read** — the Arc-ptr side tables still do all
  the work (keyed by the HashData ptr now). So it is *intended* behavior-invariant.

**BLOCKER (why 1a is not shippable alone):** the allocation-pattern shift makes
the latent Arc-ptr-reuse flaky DETERMINISTIC: `t/typed-hash-bind-typecheck.t`
test 11 fails — after `my Int %a; my Cool %b := %a;` a fresh `{a=>1}` literal
reuses the dropped Int hash's pointer and inherits its stale
`hash_type_metadata` entry, so `my Int %h := {a=>1}` wrongly type-matches and
does not throw `X::TypeCheck::Binding`. (Repro:
`my Int %a; %a<x>=1; my Cool %b := %a; my Int %h := { a => 1 }` — should throw.)
This is exactly the flaky 1b removes; 1a alone perturbs it into determinism.

## Stage 1b — embed hash *type* metadata in HashData (DONE)

**Status (2026-06-12): type metadata embedded; `hash_type_metadata` side table
deleted.** The canary `t/typed-hash-bind-typecheck.t` (11/11) and the original
repro now throw correctly; `S02-types/hash.t` is 112/112 again (it had been
SEGV-ing — see below). value_type/key_type/declared_type live in `HashData` and
travel through copy-on-write; `container_type_metadata`/`is_object_hash`/
`hash_key_type` read them directly (no side table for hashes).

Key mechanism: `Interpreter::tag_container_metadata(value, info) -> Value` embeds
the type info into the `HashData` (via `Arc::make_mut`) for hashes and falls back
to the Arc-pointer side tables for Array/Set/Bag/Mix/Instance (returning the same
Arc). Every hash-reachable `register_container_type_metadata` writer was migrated
to `tag_container_metadata` + write-back of the returned value into its owning
slot (env/local). `register_container_type_metadata`'s Hash arm is now a
`debug_assert!(false)` tripwire to catch any missed site. The name-based
`reconcile_hash_type_metadata_from_name` healing workaround is deleted (no longer
needed — embedded metadata is COW-stable).

**Latent UB found & fixed:** `hash_autovivify` / `hash_slot_ref`
(`src/value/mod.rs`) still cast `Arc::as_ptr(arc) as *mut HashMap` — reading the
`HashData` as a bare `HashMap`. This was accidentally working on the Stage-1a
branch only because `map` happened to land at offset 0; adding the
`declared_type` field reordered the struct and turned it into a SEGV. Fixed to
cast to `*mut HashData` and go through `.map`.

### Stage 2 — object-hash original_keys (DONE: embed; side tables deleted)

**Status (2026-06-12):** `original_keys` (the `.WHICH`-string → original-key-object
map) now lives in `HashData.original_keys`. Both Arc-pointer side tables are
DELETED: the interpreter's `hash_object_keys` field (+ `set_hash_object_key` /
`hash_object_keys_get` / `migrate_hash_object_keys`) and the global
`hash_original_keys_registry` in `runtime/utils.rs` (+ the `*_by_id` /
`take_*_by_id` pointer helpers). Because the map travels with the hash through
copy-on-write, the entire COW pointer-migration dance in the element-assignment
path (`migrate` + `take_by_id` + `register_by_id` after each make_mut) is GONE —
object keys are written into the same `&mut HashData` as the element insert.

- Writers: element-assignment paths set `hd.original_keys` in the same
  in-place/`make_mut` mutation; result-builders (Set/Bag/Mix→Hash coercions,
  typed-hash coercion) use `runtime::utils::set_hash_original_keys(value, keys)`.
- Readers: `hash_original_keys_snapshot` / `hash_typed_key` read
  `HashData.original_keys`; the `.keys` object-hash path reads the single source.

`make test` green; no whitelisted regression.

**Object-hash enforcement/construction follow-ups (non-whitelisted
`S09-hashes/objecthash.t`):**
- DONE (separate PR): mixin objects as keys (test 36) — `value_which_key` now has
  a `Value::Mixin` arm folding the sorted role/mixin names into the key, so
  `"x" but $r1` and `"x" but $r2` are distinct object-hash keys; the original key
  objects come back via the COW-stable `original_keys`. Also fixes the
  order-flaky test 4 in part.
- DONE (same PR): `.new` on an object-hash instance stays an object hash
  (tests 34–35) — `dispatch_new`'s Hash arm delegates to `Hash[vt,kt].new` when
  the receiver carries a `key_type`, mirroring the typed-Array `.new` path.
- STILL OPEN — key-type check rejecting a `Mu` key for `%h{Any}` (test 3, and the
  dependent order-flaky test 4). ROOT CAUSE is NOT object-hash: `Mu.new` builds an
  instance whose `.WHICH`/`value_type_name` is `Any` (`Any|46`), not `Mu`, so
  `Mu.new ~~ Any` wrongly returns True. Fix `Mu.new` construction/typing first
  (broad blast radius — smartmatch), THEN test 3 falls out for free.
- STILL OPEN — `Hash.push` key/value type checks (tests 21, 23, 24).
- DONE (#2959): the multidim Range-slice root cause of test 55
  (`%a{1;1..3}:exists` returned a single `False`) — `resolve_multidim_indices`
  now expands a `Range`/`Seq` dimension to its element list, joining the existing
  comma-list multi-index path. NOTE: test 55 still won't show green in
  objecthash.t until the abort below is fixed (it sits after it).
- STILL OPEN (BLOCKER for tests 37–62) — object-hash (and ANY hash) in list→hash
  context: `my %c = (%h,)` throws "Odd number of elements" and aborts the file
  (rakudo #5165 block, ~line 110). A naive `build_hash_from_items` `Value::Hash`
  flatten arm (#2958, REVERTED) regressed `S02-types/assigning-refs.t`: raku
  flattens a bare `%h` into pairs but must NOT flatten an itemized `$hashitem`
  (`%hash = ($hashitem,)` is meant to die odd) — and mutsu decontainerizes BOTH
  to a bare `Value::Hash` by the time `build_hash_from_items` runs, so they're
  indistinguishable there. The correct fix needs itemization tracking (preserve
  the `Scalar` wrapper for `$`-sourced hashes through list construction) — a
  container-identity Phase-2-level change. Do NOT re-add the unconditional flatten.

---
(original plan below)

The fix is to make HashData the **authoritative** source of hash metadata and
delete the side-table fallback, so a fresh hash (HashData fields = None) cannot
inherit a stale entry.

The hard part: `register_container_type_metadata(&self, value: &Value, info)`
currently *tags an existing* (often shared, borrowed) hash by pointer. To embed
metadata in HashData it must instead be set **at/near construction** (when the
Arc is uniquely owned, so `Arc::make_mut` is a no-op clone) or the call site must
own the slot and re-store the mutated hash. ~67 register call sites + the var-decl
typed-hash path.

Concrete steps:
1. `container_type_metadata` / `hash_key_type` Hash arm: read `items.value_type`
   / `items.key_type` (return early if `items.has_meta()`); REMOVE the
   `hash_type_metadata` side-table read for hashes.
2. Migrate every hash metadata writer to set `HashData` fields instead of
   `hash_type_metadata.insert`. For writers that hold the owning slot
   (`my Int %h` decl, `%h := ...`, coercions building a fresh hash), build the
   HashData with the fields set, or `Arc::make_mut(slot).value_type = ...`.
3. `register_hash_original_keys` → set `HashData.original_keys`; readers
   (`.keys`/`.raku`/subscript/object-hash WHICH lookup) read it from HashData.
   This is what makes object-hash survive COW (the classify.t fix that regressed
   on the side table — see memory `phase2-status-and-objecthash-blocker`).
4. Delete `hash_type_metadata` (+ the name-based reconcile workarounds it needed).
5. Verify: `t/typed-hash-bind-typecheck.t` (the canary), `make test`, and the
   **release roast** (Arc-ptr flaky only shows in release — S02-names-vars/perl.t,
   S02-types/hash.t). Then object-hash (classify.t/objecthash.t) as Stage 2,
   reusing the now-COW-stable original_keys.

Array/Set/Bag/Mix have the SAME Arc-ptr side tables (array_type_metadata, …) and
the same flaky; apply the same wrapper (ArrayData, …) after the hash one lands.

### Stage 3 — Set/Bag/Mix type metadata embedded (DONE, 2026-06-13)

`SetData`/`BagData`/`MixData` already existed as wrapper structs (with
`original_keys`), so no `Value` variant change was needed: added
`value_type`/`key_type`/`declared_type` + `has_type_meta()` to each,
extended `tag_container_metadata` with Set/Bag/Mix arms (same skip-if-same
COW guard preserving `.WHICH`), migrated the four QuantHash writer sites
(`SetHash[T].new` type_args, `.MixHash` coerces ×2, `is SetHash/...` var
trait) to tag + write-back, pointed `container_type_metadata` at the
embedded fields, replaced the one raw-id reader (`set_type_metadata_get`),
and deleted the three side tables. The Hash/Set/Bag/Mix arms of
`register_container_type_metadata` are a shared `debug_assert` tripwire.

### Stage 4 — ArrayData wrapper + embedded array type metadata (DONE, 2026-06-13)

`Value::Array(Arc<Vec<Value>>, ArrayKind)` → `Value::Array(Arc<ArrayData>,
ArrayKind)` with `value_type`/`key_type`/`declared_type` embedded;
`array_type_metadata` (the last per-type side table) deleted. **All five
container types now carry their type metadata in the value — the Q2
Arc-pointer-keyed type-metadata flaky class is fully closed.**

Migration notes (the Stage-1a-scale grind, ~380 compile errors / 75 files):
- Deref/DerefMut to `Vec<Value>` absorbed almost all read sites; a
  cargo-JSON-driven span rewriter handled the mechanical type mismatches and
  a second script split the ~50 `Value::Array(b, ..) | Value::Seq(b)` joint
  match arms (duplicating the arm body — the same text compiles against both
  binding types thanks to Deref).
- **Hand-audited hazards the scripts could not see** (each would have been a
  silent semantic break):
  - every `Arc::as_ptr(..) as *mut Vec<Value>` unsafe in-place site → cast to
    `*mut ArrayData` (the offset-0 UB precedent from Stage 1a);
  - `Arc`-identity-sensitive rewrites: `ArraySlotRef.array` must SHARE the
    parent arc (not copy), circular-array self-references must share
    `result_arc`, grep-view updated_source, `selected_array` ptr_eq probes;
  - `ValueIterator` gained an `ArraySlice` variant so the for-loop hot path
    keeps sharing the backing Arc (no per-iteration O(n) copy).
- `tag_container_metadata` gained the Array arm (skip-if-unchanged COW guard
  preserves `.WHICH`); `register_container_type_metadata` is now a tripwire
  for all five container types; ~16 register sites converted by script to
  tag + write-back. `reattach_array_type_metadata` re-tags by name (rebuild
  paths); plain `Arc::make_mut` mutators carry metadata automatically now.
  `capture/restore_container_meta` shrank to `is default` only;
  `unregister_container_type_metadata` deleted (untyped redeclaration clears
  the embedded meta via `clear_hash_type_metadata`, which handles arrays too).
- Bonus correctness fix: `my @copy = @typed; @copy.push(...)` no longer
  strips the SOURCE's type metadata (the old shared-side-table scrub); the
  embedded clear COW-detaches, matching raku.

Array⇄Seq conversions now copy the element vector (the Arc can no longer be
shared across the two representations); int.t / bench-class canaries show no
measurable cliff.

**Remaining candidates to embed in `ArrayData` later**: the array
`is default(...)` value and the shaped-array dims (both still pointer-keyed,
Weak-guarded side tables from #2953); the grep rw-view binding (slated for
removal with the SlotRef machinery in container-identity Stage 2).

## Why staged this way

Stage 1a (wrapper, intended-invariant) is the mechanical foundation; 1b (embed +
delete side table) is the value (kills the flaky, enables object-hash) but needs
the register-at-construction rework. Do 1b in one focused PR with the canary test
above gating it.
