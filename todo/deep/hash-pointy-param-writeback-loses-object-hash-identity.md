# A hash `for`-loop pointy-block parameter loses object-hash identity when written back through a hyper-func-op

`roast/S03-metaops/infix.t` under `MUTSU_REAL_TEST=1` (the vendored real
`Test.rakumod`, see `todo/deep/vendor-real-test-module.md`) aborts after 2086
of 5076 planned tests, 171 of them failing, all in the hash section:

```raku
for (%a, %b, %reset, ...) -> %a, %b, %reset, ... {
    is-deeply %a <<[&metaop]>> 3, %resultop3, "%a <<$name=>> 3";
    is-deeply %a, %resultop3, "%a assigned from %a <<$name=>> 3";
    ...
}
```

`%a` here is a `for`-loop pointy-block parameter bound (aliased) to one of
the outer object hashes (`%ao{Any}`, `%ac` typed, etc — see the file, lines
160-238). Real `raku` keeps `%a` (and the thing it is aliased to) an object
hash (`Hash[Any,Any]`) across the `<<[&metaop]>>` writeback:

```
$ raku -e '
my &metaop = &[~=];
my %a = "a".."e" Z=> 1..5;
my %ao{Any} = %a;
for (%ao,) -> %x {
    %x <<[&metaop]>> 3;
    say %x.WHAT;                # (Hash[Any,Any])
}
say %ao.raku;                   # (my Any %{Any} = ...) -- %ao itself mutated
'
```

mutsu instead demotes `%x` to a plain `Hash` with garbled `.WHICH`-encoded
string keys (`"Str|a"` etc rendered raw), and the outer `%ao` is never
updated at all — the two containers are not actually aliased.

## What was fixed in this pass (PR TODO)

`hyper_op_pair`'s hash-scalar branches (`vm/vm_hyper_ops.rs`, the *symbolic*
literal-operator path, e.g. `%a{Any} >>~>> 3`) built a fresh plain `HashData`
for the result, dropping `key_type`/`value_type`/`declared_type`/
`original_keys` even though the key set is untouched. Fixed to carry that
metadata over, mirroring the hash-hash branch immediately above it and the
already-correct `tagged_hash` helper in `vm_hyper_func.rs`. Pinned by
`t/hyper-hash-scalar-object-hash-type.t`. This does **not** touch
`S03-metaops/infix.t`'s failures — that file always calls through a lexical
`&op`/`&metaop` variable (`vm_hyper_func.rs`'s `exec_hyper_func_op_hash`,
already metadata-correct on its own), not the symbolic-operator path.

## The actual remaining bug: two independent, unconditional "materialize a fresh plain hash" passes

Chasing the `S03-metaops/infix.t` failures traced to `%a`'s value losing its
`key_type` on **every** store back into the loop's own bare name `%a` — both
for a genuine mutation (`do_writeback == true`, `&metaop` ends in `=`) and
for a pure no-op re-store (`do_writeback == false`, the compiler always
re-emits a store after `HyperFuncOp` when the LHS is a simple `Var`/
`HashVar`, even for a non-mutating op — see `compile_expr_hyper_func_op` in
`compiler/expr_ops.rs`).

Every `%`-assignment goes through **two** coercion passes in sequence
(`src/vm/vm_var_assign_set_local.rs:exec_set_local_op_inner`):

1. `coerce_hash_var_value` (`vm_var_assign_coerce.rs`) — for a `Hash` RHS
   with `h.has_typed_keys()`, rebuilds a fresh map with **plain-encoded**
   keys (`Value::hash_key_encode`, not `.WHICH`), while **unconditionally
   copying `key_type`/`value_type`/`declared_type`** from the source. The
   comment there explains the intent: a plain (unconstrained) target is
   supposed to see stringified keys and revert to a plain `Hash` (matching
   `raku`: `my %o{Any} = ...; my %p = %o; %p.WHAT` is `(Hash)`), while a
   *constrained* target is supposed to get re-`.WHICH`-keyed later by
   `coerce_typed_container_assignment` when `tag_container_metadata` runs.
2. `coerce_typed_container_assignment` (`vm_var_assign_typed.rs`) — called
   **unconditionally** for every `%`/`@` store
   (`vm_var_assign_set_local.rs:1145-1146`, no gate on whether the target
   actually has a declared constraint). For a hash target with
   `key_constraint == None` (our `%a`, an untyped pointy-block name), it
   rebuilds **again** via `Value::hash(coerced_map)` — a fresh `HashData`
   whose `key_type` defaults to `None` regardless of what the input carried.

For an ordinary `my %p = %o;` these two passes net out consistently by
accident: pass 1 leaves plain keys + stale `key_type: Some(..)`, pass 2 drops
`key_type` to `None` and leaves the (already-plain) keys alone — the correct
final state (`key_type: None`, plain keys). That is why
`t/object-hash-which-keys.t`'s "object hash assigned to a plain hash
stringifies keys" test (and everything else exercising a genuine fresh
declaration) already passed before touching anything.

But for the `for`-loop pointy-block case the *intended* behaviour (per the
`raku` trace above) is neither of these: `%a` is not receiving "a fresh
value" at all — it is an **alias** being mutated/re-stored through, and
should keep the aliased container's identity (including its `key_type` and,
critically, the *outer* container's own mutation) completely untouched by
either coercion pass. Neither pass has any way to distinguish "a fresh `my`
assignment" from "a write-through re-store of an existing bound alias" — both
just see `(var_name, incoming_value)`.

### An attempted fix, and why it was reverted

The direct patch — making pass 1's rebuilt map call
`ensure_object_hash_which_keys` when `key_type.is_some()` so the two passes'
key-encoding conventions actually agree — makes `S03-metaops/infix.t` pass
5076/5076 (both providers), but **regresses six `t/` files**
(`t/object-hash-which-keys.t` test 20, `t/classify-bucket-itemized.t`,
`t/classify-hash-any-mu-raku.t`, `t/classify-pair-iteration.t`,
`t/list-bind-trailing-array.t`, `t/named-array-destructure-absent-key.t`,
`t/return-coercion-lazy-gather.t`) because pass 2 still unconditionally
strips `key_type` afterward while now leaving `.WHICH`-encoded keys in place
(instead of the plain keys it was written assuming) — the exact same
invariant violation, just introduced one step later and in the *other*
direction. Reverted; not shipped.

### Update (2026-08-18, planning pass): the `inplace_old_hash` theory above was wrong — the real fault is a missing `%`-writethrough helper in the ContainerRef cell branch

A dedicated planning pass (read-only investigation, no code changed) found that
the `quanthash_store_preserving_identity` / `inplace_old_hash` mechanism
described above **never fires for the failing case** — `inplace_old_hash` is
captured only when the local slot holds a `ValueView::Hash` directly
(`vm_var_assign_set_local.rs:662-672`), but by the time the roast file's `%a`
breaks, the slot no longer holds a bare hash at all.

**What already works today (confirmed by direct gdb/repro testing, do not
touch):** a multi-param `for (%ao,…) -> %a,… { }` binds `%a` via a
bind-flavored `SetLocal` (`is_bind=true`) that keeps the *same*
`Gc<HashData>` node as `%ao` and registers `var_type_constraint("%a")`
(`vm_var_assign_set_local.rs:705-711`, `:1719-1741`). A later plain
`%a = %reset` on that fresh binding correctly demotes-then-re-tags through
`tag_container_metadata`/`ensure_object_hash_which_keys`
(`runtime/runtime_container.rs:112-123`) and writes back into the *original*
shared node via `hash_inplace_reassign` (`vm_var_assign_set_local.rs:1794-1799`)
— `%ao` observes the mutation correctly in this path today.

**The actual failing lane.** The roast failures all begin *after* the first
`is-deeply %a, ...` call in the loop body — i.e. after `%a` has been passed
once as an argument to a `$`-sigil (scalar) parameter:

1. Passing `%a` into a `Mu $got`-style parameter triggers the existing
   "Slice 2d" scalar-container-share promotion
   (`runtime/types/binding_signature.rs:2149-2158`): the value is wrapped in a
   fresh `ContainerRef` cell (recorded in `rw_bindings`), and on return the
   caller's `%a` local slot is replaced by that cell — inner value still the
   original object-hash node.
2. Every later whole-store to `%a` (a plain re-assignment, or the compiler's
   writeback store after `HyperFuncOp` — `compile_expr_hyper_func_op` in
   `compiler/expr_ops.rs:307-338`) now hits the **ContainerRef branch** of
   `exec_set_local_op_inner`, not the bare-hash `inplace_old_hash` branch.
   At `vm_var_assign_set_local.rs:1618-1649` this branch:
   - calls `array_container_writethrough_value`
     (`vm_var_assign_typed.rs:13-80`) — a **no-op for `%` names** (returns
     `raw` unchanged at `:19-21`); there is no hash analogue,
   - stores via plain `Value::store_through_cell(&arc, &val)` (`:1646`),
     which replaces the cell's inner value wholesale, **orphaning** the
     `Gc<HashData>` node that `%ao` still holds,
   - **returns early at `:1648`**, skipping both the constraint re-tag
     (`:1761`) and the `hash_inplace_reassign` identity write-back
     (`:1794`) that the "already works" path above relies on.

   Net effect: the value that reaches this branch has *already* been
   demoted by pass 1 + pass 2 (plain keys, `key_type` stripped), and nothing
   in this branch restores it — `%a` becomes a plain `Hash`, and `%ao` is
   silently detached and stale.

A minimal repro pair confirms this precisely (verified on
commit `238a20099`, gdb breakpoints at `vm_var_assign_coerce.rs:238`,
`vm_var_assign_typed.rs:236`, and `vm_var_assign_set_local.rs:1646` /
`:1799` / `:1812`):

```raku
sub my-check(Mu $got) { 1 }
my %h = "a".."e" Z=> 1..5;
my %ao{Any} = %h;
my %reseto{Any} = "a".."e" Z=> 100..104;
for (%ao,) -> %a {
    my-check(%a);          # Slice-2d boxes %a into a ContainerRef cell
    %a = %reseto;          # cell branch: demoted + %ao left stale (WRONG)
    say %a.raku;           # {:a(100),...} plain Hash, not Hash[Any,Any]
}
say %ao.raku;              # unchanged — should show the reset values
```

Deleting the `my-check(%a)` line makes both correct today. This also
explains why the roast file's failures start partway through the file: every
subtest section calls `is-deeply %a <<[&metaop]>> 3, ...` (an `is-deeply`
call boxes `%a` on its *first* invocation), and only stores *after* that
point are affected.

A secondary, lower-impact issue exists in the single-param loop-writeback
lane (`for (%h,) -> %x { %x = ... }`, no prior scalar-arg call): that store
goes through the **name-based SetGlobal lane**
(`vm/vm_exec_dispatch.rs`, roughly `:1182-1256`) instead of a compiled local
slot. There, pass 2 is gated on a declared constraint, so pass 1's
`key_type` survives — but pass 1 also leaves the keys **plain-encoded**
while `key_type` is set, violating the object-hash "every key stored under
its `.WHICH` string" invariant (`runtime/utils.rs:292-328`). Plain-key
lookups still resolve (`has_typed_keys` tolerates it,
`value/value_collections.rs:83-88`), but this is latent denormalization
that a fix should also close, ideally by extending this lane's `%` arm to
mirror the `@` arm's existing `array_container_writethrough_value` call
(`vm_exec_dispatch.rs:1236-1256`).

### Proposed fix (no new `unsafe`, reuses existing audited mechanisms)

1. Add `hash_container_writethrough_value` to `vm_var_assign_typed.rs`,
   next to `array_container_writethrough_value` (`:13-80`) — the `%`
   counterpart: re-tag the incoming value's metadata via
   `tag_container_metadata`, preferring a declared constraint
   (`var_type_constraint`/`var_hash_key_constraint`) and otherwise
   inheriting the *existing* cell value's own container-type metadata
   (`container_type_metadata(old)`) when it is itself an object hash. For a
   plain-hash `old`, return the value unchanged (preserves today's
   fresh-`my`-demote behavior byte-for-byte).
2. In the ContainerRef branch of `exec_set_local_op_inner`
   (`vm_var_assign_set_local.rs:1618-1649`): call the new hash helper
   alongside the existing array helper, then replace the unconditional
   `Value::store_through_cell` for `@`/`%` names with the already-existing
   `Self::cell_store_preserving_container_identity` helper
   (`vm_var_assign_ops.rs:729-758`), which itself dispatches to the already
   audited `hash_inplace_reassign`/`array_inplace_reassign` when the inner
   and incoming values are the same container kind with different `Gc`
   nodes — the same mechanism the SetGlobal lane already uses elsewhere in
   this file. Keep the `$`-sigil (scalar) arm on plain `store_through_cell`
   — do not touch scalar cell semantics.
3. (Optional, separate commit) extend the SetGlobal `%` arm
   (`vm_exec_dispatch.rs:1231-1256`) to call the same new helper for the
   unconstrained case, closing the secondary single-param-loop
   denormalization described above.

Neither pass 1 (`coerce_hash_var_value`) nor pass 2
(`coerce_typed_container_assignment`) needs to change at all under this
plan — so the six `t/` files the earlier reverted attempt regressed
(`t/object-hash-which-keys.t` test 20, `t/classify-bucket-itemized.t`,
`t/classify-hash-any-mu-raku.t`, `t/classify-pair-iteration.t`,
`t/list-bind-trailing-array.t`, `t/named-array-destructure-absent-key.t`,
`t/return-coercion-lazy-gather.t`) should be structurally unaffected: their
common dependency is that a genuine fresh `my %p = %objhash;` declaration
(`is_vardecl=true`, no prior cell-boxing) nets out to "plain keys,
`key_type: None`" exactly as it does today, since this plan does not touch
that code path.

**Unsafe-aliasing note:** this plan introduces no new `unsafe` and no new
caller of `gc_contents_mut` — it only routes one additional call site
through the already-existing, already-audited
`cell_store_preserving_container_identity` → `hash_inplace_reassign` chain
(`vm_var_assign_ops.rs:729-780`). One thing to double check when
implementing: the `old` snapshot in the ContainerRef branch is taken via
`arc.lock().unwrap().clone()`, and the lock guard must be dropped (as the
existing code already does, by ending the `.clone()` expression) *before*
`cell_store_preserving_container_identity` re-locks the same `Mutex`, to
avoid a self-deadlock.

### Step-by-step implementation order

1. Reproduce the repro pair above on the implementation branch first, and
   confirm the failure with the gdb breakpoints listed.
2. Implement the `hash_container_writethrough_value` helper (change 1) and
   wire it into the ContainerRef branch (change 2). Build.
3. Re-run the repro pair — both `%a` and `%ao` should show
   `Hash[Any,Any]`/the reset values.
4. Run the targeted regression sweep before touching roast:
   - the six previously-regressed files listed above,
   - `t/hyper-hash-scalar-object-hash-type.t` (the already-shipped pin),
   - cell/write-through surface: `t/scalar-param-container-share.t`,
     `t/scalar-param-container-share-method.t`,
     `t/named-param-container-share.t`, `t/element-array-share.t`,
     `t/scalar-array-share.t`, `t/hash-bind-cell.t`,
     `t/hash-entry-ref-deep-bind.t`, `t/container-identity-mutation.t`,
     `t/container-identity-whole-assign.t`,
     `t/container-identity-phase2-complete.t`,
   - for-loop param machinery: `t/for-multi-param-writethrough-metadata.t`,
     `t/for-multiparam-copy-rw.t`, `t/for-multi-param-shared-lane.t`,
     `t/for-multi-param-type-constraint.t`, `t/for-loop-named-param-alias.t`,
     `t/for-pairs-value-quanthash-writeback.t`,
     `t/for-quanthash-values-rw-writeback.t`, `t/closure-rw-param-writeback.t`,
   - hyper hash: `t/hyper-hash-ops.t`, `t/hyper-hash-method-scalar.t`,
     `t/hyper-itemized-hash.t`, `t/hyper-op-nested-hash.t`.
5. Run the roast file both providers:
   `MUTSU_FUDGE=1 MUTSU_REAL_TEST=1 timeout 600 ./target/debug/mutsu roast/S03-metaops/infix.t`
   and `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/S03-metaops/infix.t`
   — expect 5076/5076. If a residue remains, gdb it rather than reaching back
   into pass 1/pass 2.
6. Add a pin test `t/hash-cell-writethrough-object-hash.t` covering: a
   call-boxed loop param keeping `Hash[Any,Any]` and writing through to the
   outer object hash; the same with a `Cool`-typed hash (`value_type` only);
   a plain-hash celled store staying plain (unchanged behavior); and the
   metaop-writeback shape from the roast file itself.
7. Optionally implement the SetGlobal-lane change (change 3) as a follow-up
   commit, re-running the same sweep plus `t/hash-key-type-object.t`,
   `t/hash-key-type-object-list.t`, `t/object-hash-which-keys.t`.
8. `make test`, then push and let CI's `make roast` be the full backstop
   (see the repo's roast-delegation convention).

### Open risks / unknowns for the implementer

- Whether change 2 alone clears all 171 subtests, or a residual cluster
  remains (e.g. around `Cool`-typed iteration or a specific op group) —
  wasn't verified against a patched binary in this planning pass (read-only
  investigation). Use the gdb breakpoints above to chase any residue.
- Change 2 also switches `@` names in the same ContainerRef branch onto the
  identity-preserving store (today only `%` is broken, but the branch is
  shared). This should align with existing array aliasing semantics, but if
  the array `t/` sweep in step 4 objects, land the identity-preserving store
  gated to `%` names only as a first cut.
- `%ao`'s own mutation through the `%a` alias is not asserted by the roast
  file at all (it only checks `%a`'s value) — the pin test in step 6 is the
  only thing that will catch a regression on that half again.
- Change 3 (SetGlobal lane) is deliberately optional/separate: it touches
  closure-captured and `our`-hash writes too, so verify its own sweep
  cleanly before folding it in.

## Where this sits in the `vendor-real-test-module.md` campaign

`S03-metaops/infix.t` is (as of 2026-08-18) the single largest cluster in
that campaign's roast residue (171 of the total ~148 raw / 141 genuine
regressed files' worth of failing subtests come from this one file alone —
see that ticket's latest sweep). Fixing this unblocks the file outright
(verified: with the reverted patch in, `MUTSU_FUDGE=1 MUTSU_REAL_TEST=1
target/debug/mutsu roast/S03-metaops/infix.t` ran clean 5076/5076 under both
providers) and is worth prioritizing on a fresh session with room for the
`unsafe`-audit work.
