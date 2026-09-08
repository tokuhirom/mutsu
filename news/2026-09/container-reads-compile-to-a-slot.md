# `@`/`%` reads compile to a slot — ADR-0039 slice 2 lands

A container read (`Expr::ArrayVar` / `Expr::HashVar`) used to compile to a
by-name `GetArrayVar` / `GetHashVar` where a scalar read compiles to
`GetLocal(slot)`. Container lexical scoping was therefore *dynamic*: every
same-named declaration anywhere in the process — a consumer's `my`, an inner
block's shadow, a sibling routine's local — could hijack the name, because the
read re-resolved it against whatever `env` happened to hold. A plain user
lexical container now resolves through its slot, exactly as a scalar does.

This is ADR-0039 §4.2's first bullet, and the fourth attempt at it. Attempts 1-3
were each withdrawn; §12 recorded attempt 3's seven repairs in enough detail to
be **re-derived rather than re-measured**, which is what this one did.

## Why the by-name read had to go

It was not only wrong in itself, it *hid store-lane bugs*: two paths that end up
naming different containers under one name look fine as long as every read
re-resolves the name. Six of the repairs below are real bug fixes that simply had
no observable symptom while the read stayed by-name.

## The restriction is a requirement, not a safety margin

The flip covers **plain user lexicals** only (`env::is_plain_user_lexical` on the
sigiled name). The by-name read's tail is load-bearing for every other shape: its
`None` arm supplies the empty container that makes `%_` read as `{}` on the fast
method-dispatch path that deliberately leaves it unbound, and its cascade is the
only route by which `%!attr`/`%.attr` reach `self`'s attribute cell and `@*dyn` /
`%?RESOURCES` / `::`-qualified names resolve at all. Those are not lexical
bindings of the frame; slice 2 is about the ones that are. Any future widening
has to supply those behaviours first.

## What was repaired

1. **`GetLocal` must not prefer a name-keyed store over this frame's own
   binding.** Two separate corrections. Its `@`/`%` arms consult the
   `__mutsu_atomic_*` lanes before the slot; when the slot holds the very
   `ContainerRef` cell that `env` names, that ordering is backwards, so the lane
   probe is skipped on cell identity. And its *bare-name* `shared_vars` probe was
   an unconditional **preference** where the by-name read it replaces treats the
   same store as a **fallback** (`get_env_with_main_alias_inner` gates its
   base-name probe on `is_thread_clone`, reading `env` first otherwise). It is
   now gated the same way; the Nil arm at the end of the op already supplies the
   fallback.
2. **An expression-position container declaration takes the slot the read
   resolves to.** `local_map` is monotonic, so a popped sibling block's `@a` slot
   stays reachable and `{ my @a = 5,7,9 } (my @a).push: $_ for ^3` stored into
   `env` alone.
3. **`try_fast_hash_element_assign` uses its compiler-baked `target_slot`.**
   `find_local_slot` is a `position` search, so with a same-named shadow
   (`code.locals == ["%h", "%h"]`) it nil'd and re-seeded the OUTER binding.
4. **`write_back_hyper_target_var` writes THROUGH the container node.** Its
   fallback did `set_env_with_main_alias` plus a by-name `locals_set_by_name`,
   which under a shadow wrote the outer slot and made `@r»++` answer `1 2 3`.
   Mutating the existing node reaches every holder and needs no slot search —
   which is why a baked `HyperMethodCall` target slot turned out to be
   unnecessary. Restricted to an `@`/`%` target: a scalar-held QuantHash
   (`$b>>--`) RETURNS the original, and that return value shares the node.
5. **The `is BagHash`/`SetHash`/`MixHash` trait handler re-syncs its slot** after
   registering the name-keyed constraint, which re-tags `env`'s value through
   `Gc::make_mut` and so COPIES a node the slot shares.
6. **`overwrite_{array,hash}_bindings_by_identity` preserve container
   identity.** They replaced every `env` entry holding the old node with a fresh
   one and relied on a name-keyed writeback drain to reach the local slots. They
   now copy the rebuild into the original node first, so a local slot, a
   by-value capture and a `:=` alias all observe an element store made through
   an accessor.

`store_container_preserving_identity`'s Set/Bag/Mix arms — attempt 3's seventh
repair — had already shipped on their own.

## The blocker attempt 3 left, and the one this attempt found

Attempt 3 reached a green `prove t/` and a green `make roast` and was stopped by
the bundled-library battery gate on `Cro::HTTP/router-auth.rakutest` and
`zef/distribution-depends-parsing.rakutest`. Its diagnosis — a `gather` body's
container append reaching the atomic lane instead of the owner's cell — did not
survive this attempt's differently-derived repairs: the zef file passes
unchanged, and the Cro file failed for an unrelated reason.

The reduction harness §12 asked for was rebuilt and is kept:
`MUTSU_SLOT_READ_DUMP` prints every name the flip applies to, and
`MUTSU_SLOT_READ_FILTER` restricts the flip to a comma-separated list of them.
Delta debugging over the 95 names a Cro run touches reduced the failure to a
single one, `@middleware`, in about four minutes — after which a slot-vs-env
probe showed the slot and `env` agreeing on this frame's binding in all 36 calls
while `shared_vars` held a *different* handler's array, and `GetLocal` answered
the store in 17 of them. Repair 1's second half is that finding. Note the shape:
a plain non-slurpy `@` parameter is deliberately left unmasked by
`mask_thread_redeclared_params` precisely so its bare-name entry can serve as a
*fallback* for a nested spawn — which is exactly why preferring it was wrong.

## Acceptance

`prove t/` (3861 files, 40929 tests), a full local `make roast`, and
`MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh` are all green with
the flip on. `t/nested-frame-container-mutation-reaches-owner.t` and
`t/container-lexical-declarator-matrix.t` keep passing, and the repairs above are
pinned by `t/container-slot-read-repairs.t` plus
`t/attribute-accessor-container-identity.t`.
