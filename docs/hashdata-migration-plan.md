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

## Stage 1b — embed hash metadata in HashData (the remaining work)

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

## Why staged this way

Stage 1a (wrapper, intended-invariant) is the mechanical foundation; 1b (embed +
delete side table) is the value (kills the flaky, enables object-hash) but needs
the register-at-construction rework. Do 1b in one focused PR with the canary test
above gating it.
