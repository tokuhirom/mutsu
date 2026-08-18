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

### The real fix needs container-identity write-through, not key-encoding patching

This codebase already has the right pattern for the underlying problem —
`Interpreter::quanthash_store_preserving_identity` in
`vm_var_assign_coerce.rs` (used for `my %s is SetHash = ...`
re-declaration): when the *existing* value at a name is the same kind of
container as the incoming one, it writes the new contents **into the
existing `Gc` node in place** (`unsafe { *gc_contents_mut(&old) = data; }`,
under the audited aliasing contract in `gc/gc_ptr.rs`'s `gc_contents_mut`
docs / ADR-0013 §8), so every other holder of that same node — env mirrors,
`:=` binds, closures — observes the mutation, instead of returning a
disconnected fresh value that only the local slot sees.

The likely correct fix is the hash analogue of that helper, gated the same
way `inplace_old_hash` already is (`!is_bind && !is_rebind && !is_vardecl &&
!is_anon_container` — i.e. skip for a genuine fresh `my` declaration, which
is exactly where the "materialize plain" behavior is *supposed* to fire):
when the *existing* value at this local slot is already an object hash
(`inplace_old_hash.key_type.is_some()`), write the new key/value pairs into
that **same** `Gc<HashData>` node instead of building a fresh one via either
coercion pass, and skip both `coerce_hash_var_value`'s "materialize fresh
entries" branch and `coerce_typed_container_assignment`'s unconditional
rebuild for that store (a `bool` flag threaded from the identity-preserving
branch through to the `!skip && ...` guard around the
`coerce_typed_container_assignment` call would do it). That also happens to
be the fix that would make `%ao` itself observe the mutation performed
through its `%x` alias (still broken today; not even the reverted attempt
above fixed that half — the roast test only asserts on `%a`'s own value, not
`%ao`'s, so it does not currently catch this).

This was not attempted in this pass: it requires auditing the `unsafe`
aliasing contract carefully against every hash-value caller
(`gc_contents_mut`'s docs are explicit that "no other `&`/`&mut` into this
value is *dereferenced* for the lifetime of the returned borrow" on this
thread), which is a correctness-critical, easy-to-get-subtly-wrong change
and deserved a dedicated session rather than being folded into an unrelated
hash-hyperop-metadata fix.

## Where this sits in the `vendor-real-test-module.md` campaign

`S03-metaops/infix.t` is (as of 2026-08-18) the single largest cluster in
that campaign's roast residue (171 of the total ~148 raw / 141 genuine
regressed files' worth of failing subtests come from this one file alone —
see that ticket's latest sweep). Fixing this unblocks the file outright
(verified: with the reverted patch in, `MUTSU_FUDGE=1 MUTSU_REAL_TEST=1
target/debug/mutsu roast/S03-metaops/infix.t` ran clean 5076/5076 under both
providers) and is worth prioritizing on a fresh session with room for the
`unsafe`-audit work.
