# `__mutsu_*` metadata keys get one constructor, and a ratchet

Building an env key with `format!` and then probing `Env` with it has been the
profiling finding in **five separate perf campaigns**:

| when | where | what the profile said |
| --- | --- | --- |
| 2026-08 | `bench-ctor-map-compile-and-atomic-lane-probe` | the atomic-lane probe, per construction |
| 2026-09 | `closure-call-hot-path-key-memoization` ([#7571](https://github.com/tokuhirom/mutsu/issues/7571)) | rebuilding unchanging key strings per closure call, ~3.2% of the RIPEMD run |
| 2026-09 | `typed-lexical-metadata-probes-take-symbols` ([#7766](https://github.com/tokuhirom/mutsu/issues/7766)) | `__mutsu_type::<name>` probed up to seven times per store |
| 2026-09 | `bench-ctor-name-rederivation` | 732,232 `Symbol::intern` calls for 5000 constructions — 146 per object |
| 2026-09 | [#8069](https://github.com/tokuhirom/mutsu/issues/8069) | 22 interns and 8 heap allocations per `@a[$i] = $v` |

Each was fixed by memoizing the one key that profile happened to walk through.
The pattern kept coming back, and the reason was measurable: **96 call sites
built a key by hand for which a memoized constructor already existed.** Seven
`*_key_for_sym` helpers had accumulated in `runtime_var_meta.rs` and
`shared_store.rs`, each with its own copy of the same thread-local memo
boilerplate, and nothing pointed a new site at them.

## One constructor

`src/runtime/meta_ns.rs` introduces `MetaNs`: the namespaces as an enum, and
`MetaNs::key(name_sym)` as the only way to obtain a key `Symbol`. One memo table
keyed by `(namespace, name)` replaces seven; the seven old helpers survive as
one-line delegations, so their callers did not have to move. Adding a namespace
is now one line in one `match`, and getting the memoization right is not
something anyone has to remember.

The hot sites of #8069 moved over. The element store's alias probe, which runs on
**every** element store, went from

```rust
let alias_key = format!("__mutsu_sigilless_alias::{}", original_var_name);
self.env().get(&alias_key)
```

— a heap allocation, the whole `core::fmt` machinery, a hash of the ~40-byte
result to intern it, and a matching free — to

```rust
self.env().get_sym(MetaNs::SigillessAlias.key(original_var_sym))
```

which hashes nothing after the first call. The store path also gained a
`var_sym`: the symbol its metadata keys derive from, reusing the constant-pool
symbol in the overwhelmingly common no-alias case, so the hot path interns no
string at all and only a *resolved alias* (a genuinely different name from the
one the bytecode names) has to. `__mutsu_shaped_array_dims::`, two
`__mutsu_bound::` probes and `__mutsu_bound_index::` moved with it, along with
the sigilless-alias chains in `vm_misc_assign.rs` and `vm_env_helpers.rs`.

Measured on the deterministic instruction-count series added in the same batch
(`news/2026-09/deterministic-instruction-count-bench-series.md`), which is the
only metric in the repo that can resolve a change this size:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| 500000 `@c[$i] = $i` stores | 10,249,295,393 | 9,719,805,922 | **−5.17%** |
| `bench-index-store` | 13,275,156,905 | 12,743,882,240 | **−4.00%** |
| `bench-threads-serial` | 6,880,126,866 | 6,805,773,324 | **−1.08%** |

1,059 instructions per element store, out of #8069's measured ~14,053.

## A ratchet, not a ban

276 hand-built sites remain across 91 files. `scripts/check-magic-keys.sh`
counts them per file against `scripts/magic-keys-baseline.tsv` and fails when a
count goes **up**, or when a file not in the baseline introduces one. Down is
always fine — that is the work — and `--update` re-cuts the baseline after a
conversion. It runs as `make check-magic-keys`, a `make test` prerequisite and a
CI step beside the three other ledger checks.

A ratchet rather than a prohibition, and mechanical rather than a line in
CLAUDE.md, for one reason: **every one of the five findings above was written by
someone who did not know the site was hot.** A prose rule would not have stopped
any of them. A baseline that can only shrink keeps the debt visible, stops it
growing, and needs nobody to be clairvoyant.

## What this is not

It is not the fix. Every one of these keys is a property of a single binding,
stored as a sibling entry in the same string-keyed `Env` as the variable itself
under a name derived from the variable's name — which is *why* it has to be
rebuilt and re-probed. Memoizing the construction makes the current design
cheap, which is a smaller good than deleting it. The metadata belongs on the
binding's resolved descriptor (#8069 §4.1) or off the per-frame env entirely
([#7817](https://github.com/tokuhirom/mutsu/issues/7817) / ADR-0084), and
`MetaNs` is the staging ground for that: with every access funnelled through one
enum, retiring a namespace becomes a change at one site instead of at
twenty-eight.
