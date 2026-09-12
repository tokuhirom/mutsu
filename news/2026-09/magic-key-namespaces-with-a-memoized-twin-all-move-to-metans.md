# Every `__mutsu_*` namespace that already had a memoized constructor now uses it

[#8087](https://github.com/tokuhirom/mutsu/issues/8087) counted the damage from one habit:
`format!("__mutsu_<ns>::{name}")` followed by an `Env` probe had been *the* profiling finding in
five separate perf campaigns, and each one fixed only the sites its own profile happened to walk
through. Stage 1 (#8104) built `MetaNs` — one memoizing constructor, `(namespace, name) -> key
Symbol`, with no public string form — folded the seven ad-hoc `*_key_for_sym` helpers onto it, and
wired `scripts/check-magic-keys.sh` as a per-file ratchet over the **276** hand-built sites that
remained.

This is stage 2: the sites whose namespace already had a memoized replacement. There were 102 of
them, and every one of those eleven namespaces is now at zero across `src/`.

| namespace | hand-built sites before | after |
| --- | ---: | ---: |
| `__mutsu_sigilless_alias::` | 24 | 0 |
| `__mutsu_sigilless_readonly::` | 20 | 0 |
| `__mutsu_atomic_arr::` | 17 | 0 |
| `__mutsu_callable_id::` | 12 | 0 |
| `__mutsu_atomic_hash::` | 9 | 0 |
| `__mutsu_bound_index::` | 6 | 0 |
| `__mutsu_shaped_array_dims::` | 4 | 0 |
| `__mutsu_type::` | 3 | 0 |
| `__mutsu_bound::` | 3 | 0 |
| `__mutsu_hash_key_type::` | 2 | 0 |
| `__mutsu_state_key::` | 2 | 0 |

## The conversion is not only "call the helper"

Calling the memoized constructor removes the `format!`, but a site that then probes the env *by
name* still hashes the 30-odd-byte key string on every access. So each converted site now holds the
key as a pre-interned `Symbol` and probes with `get_sym` / `contains_key_sym` / `remove_sym` /
`get_mut_sym`; neither the string nor its hash is rebuilt. `runtime::utils`'s `sigilless_alias_key`
and `sigilless_readonly_key` return a `Symbol` for that reason, and `types::signature`'s duplicate
pair of the same two functions is re-exported from there rather than being a second definition of
the key shape.

Two memo tables that had grown up beside `MetaNs` fold into it:

- `shared_store::atomic_lane_key` carried its own pair of thread-locals for the array and hash
  lanes. Those are now the `AtomicArr` / `AtomicHash` namespaces on the one table;
  `atomic_lane_str_key` is the `&'static str` form the shared store needs, since it is keyed by
  `&str` rather than by `Symbol`.
- `callable_id_key_for_syms` had its own `(package, name)` table. That becomes `MetaNs::key_pair`,
  the general two-part form — `CallableId` is the one namespace not keyed by a single name.

## The hazard this had to avoid

`Env::insert_sym` deliberately does **not** run `note_env_key`, the latch that arms the
"could a key of this family exist?" probes in `src/env.rs`; its ordinary callers write plain lexical
names, and the latch is exactly the gate that lets a program with no sigilless binding skip the
readonly probe on every assignment. A name-derived write that lands on `insert_sym` therefore stores
the metadata and then leaves the reader permanently switched off — no type error, no panic, just a
constraint or an alias that is silently never found again.

Every converted write goes through `insert_sym_noting`. To make that testable rather than merely
asserted, `t/vm/binding/var-metadata-key-namespaces.t` drives each namespace end-to-end through
behaviour that fails if the key is written but not read back: a sigilless term refusing assignment,
a two-hop `:=` alias chain, a bound element surviving a later plain element store, a shaped array's
declared dimensions, a typed lexical and an object hash refusing a wrong-typed store, a `state`
cell per closure clone, two same-named subs in different packages keeping separate state, a raw
parameter writing back to its caller, and concurrent pushes landing in a shared array and hash.
All 23 cases pass under rakudo too. `meta_ns.rs`'s own unit tests spell out the exact key every
namespace produces, so a prefix typo is caught by `cargo test` rather than by a lost constraint.

## Measured

On the deterministic instruction-count series (`scripts/bench-det.sh`, #8085 — callgrind simulation,
~0.1% resolution, against the same-toolchain `origin/main` binary):

| benchmark | JIT off | JIT on |
| --- | ---: | ---: |
| `bench-threads-serial` | **−3.71%** | **−4.15%** |
| `bench-threads` | **−2.92%** | **−3.22%** |
| `bench-ctor` | −0.30% | −0.30% |
| `bench-index-store` | −0.07% | −0.05% |
| `bench-hash` | −0.04% | −0.03% |
| `bench-grammar-parse` | +0.04% | −0.16% |

The concurrency rows are the atomic-lane keys: once any `start` block pushes to a shared array the
lane probe is armed for the rest of the process, and every `@`/`%` access after that was building
its lane key from scratch. `bench-grammar-parse` is flat at the noise floor, which is the expected
shape — a program that declares no sigilless binding, no shaped array and no atomic lane has its
probes latched off and never builds these keys at all. Nothing regressed.

## What is left

The ratchet is re-cut at **174** sites, in the namespaces that still have no memoized constructor
(`__mutsu_atomic_name::`, `__mutsu_deleted_index::`, `__mutsu_bound_decont::`,
`__mutsu_shared_dirty::`, `__mutsu_ro_index::`, `__mutsu_elem_share::` and the rest). Four of the
old 276 were not sites at all but doc comments quoting the `format!` they were describing; those
are reworded, so every number the ratchet now reports is a real construction.

Stage 3 is those remaining namespaces. Stage 4 is the actual answer, which `MetaNs` is only the
staging ground for: these keys should not exist. Each is a property of one binding, stored as a
sibling entry in the same env under a name derived from the binding's own name, which is the entire
reason it has to be rebuilt and re-probed. Move them onto the binding's resolved descriptor
([#8069](https://github.com/tokuhirom/mutsu/issues/8069) §4.1) or off the per-frame env
([#7817](https://github.com/tokuhirom/mutsu/issues/7817) / ADR-0084) and the probe becomes a field
read — with every access now funnelled through one enum, retiring a namespace is a change at one
site instead of at twenty-eight.
