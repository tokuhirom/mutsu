# Periodic maintenance runbook

mutsu depends on two kinds of external code that drift over time and need
deliberate, periodic refresh:

1. **Cargo crate dependencies** (`Cargo.toml` / `Cargo.lock`) — the Rust
   libraries we build against.
2. **Vendored upstream Raku trees** (`roast/`, `raku-doc/`, `old-design-docs/`)
   and the **bundled zef** (`vendor/zef/`) — copied-in upstream sources.

Neither updates itself. This document is the checklist for keeping them current.
It complements [`docs/vendoring.md`](vendoring.md) (the mechanics of the vendored
trees) and the CLAUDE.md "mzef … / Distribution" notes (the zef tree).

## Suggested cadence

- **Monthly (light):** run the two "check" commands below (`cargo update
  --dry-run`, `scripts/update-vendor.sh --check`) and apply the low-risk,
  in-range updates. This is a 15-minute chore that keeps drift small.
- **Quarterly (full):** additionally audit major-behind crates, re-vendor
  `raku-doc` / `old-design-docs`, and consider a `roast` re-vendor (which has
  test-suite follow-up — see below). Bump the Rust toolchain if a dependency now
  requires a newer one.

Cadence is a guideline, not a gate — pull an update forward whenever a bug fix,
security advisory, or a feature you need lands upstream.

## 1. Cargo dependency maintenance

### 1a. Audit what is behind

```sh
# Semver-compatible updates already allowed by the version ranges in Cargo.toml
# (these are safe; the lockfile just hasn't moved yet):
cargo update --dry-run

# Major-behind crates are NOT shown by the above (they exceed the pinned major).
# `cargo outdated` surfaces them, or check each direct dep against crates.io:
cargo install cargo-outdated   # once
cargo outdated -R
```

The `(available: X)` annotations in `cargo update --dry-run` output also flag a
few crates that have a newer major available.

### 1b. Apply the safe tier

```sh
cargo update            # take every in-range update
cargo build && cargo clippy -- -D warnings && make test
```

These cannot break the build barring an upstream bug; CI is the backstop.

### 1c. Major-version (breaking) bumps

For each crate a **major** behind, decide case by case — do **not** bump blindly:

1. **Read the changelog / release notes** for the breaking changes.
2. **Check our actual usage** (`grep` the crate path in `src/`). A crate used in
   one file with a tiny API surface is usually a trivial bump; a core crate
   (e.g. `num-bigint`, used in ~80 files) needs care even if its own breaking
   change doesn't touch our code path.
3. **Bump, build, fix, verify** (`cargo build` → fix call sites → `clippy` →
   `make test` → exercise the affected feature end-to-end).
4. **One concern per PR.** Keep the safe in-range sweep, each risky major bump,
   and any MSRV bump as separate PRs so a regression is easy to bisect and
   revert. Dependency PRs are `chore(deps):` and take the default patch bump (no
   tagpr label).

Worked examples from the 2026-07 refresh:

- **Trivial major bumps** (zero code change): `fancy-regex`, `unicode_names2`,
  `emojis`, `rustyline`, `libloading`.
- **Breaking change that missed our code path**: `num-bigint` 0.4 → 0.5 — the
  only break was its `rand`-feature restructuring; we enable only `serde`, so
  `BigInt`'s API was unaffected.
- **Real code migration**: `bincode` 1 → 2 — the top-level
  `serialize`/`deserialize` free functions were removed for
  `bincode::serde::{encode_to_vec,decode_from_slice}` + a config; the on-disk
  cache format changed, so the cache magic was bumped to invalidate stale files.
  (Note: `bincode` 3.0.0 on crates.io is featureless and not the serde-based
  line — 2.x is the correct target.)
- **Toolchain-gated bump**: `libffi` 3 → 5 fixed the macOS arm64 release build
  (see [ADR-0012](adr/0012-libffi-macos-arm64-vendored-bump.md)); `cranelift`
  0.132 → 0.133 required raising the Rust MSRV (next section).

### 1d. Rust version / toolchain coupling

Some crates raise the **minimum supported Rust version**. When a bump reports
`requires Rust 1.NN`, update all of these together, in their own PR:

- `Cargo.toml` `rust-version` — set to the true floor the dependency demands.
- The toolchain pins in the workflows. They are **not** all the same and must be
  kept ≥ the MSRV:
  - `.github/workflows/ci.yml`, `bench.yml` — the test/roast/bench builders.
  - `.github/workflows/release.yml`, `pages.yml`, `tagpr.yml` — the
    release/docs/version-bump builders. `tagpr.yml` is **SHA-pinned** (with a
    `# <version>` comment); resolve the new tag's commit with
    `gh api repos/dtolnay/rust-toolchain/commits/<version> --jq .sha`.

Keeping every builder on one toolchain (the version CI already validates) avoids
a green `make test` on ci.yml while `release.yml` fails on an older pin — which
is exactly how a broken release can ship unnoticed.

Verify a JIT-affecting toolchain/cranelift bump with `MUTSU_JIT=on` on a hot
numeric loop (int and f64 paths), not just `make test`.

## 2. Vendored upstream trees

Full mechanics live in [`docs/vendoring.md`](vendoring.md); the maintenance
essentials:

```sh
# How far behind upstream is each vendored tree? (no writes)
scripts/update-vendor.sh --check

# Re-vendor a doc-only tree at its tracked branch HEAD (updates vendor.lock):
scripts/update-vendor.sh raku-doc
scripts/update-vendor.sh old-design-docs
```

- **`raku-doc` / `old-design-docs`** are documentation-only; re-vendoring them is
  low-risk. Do them in their own PR (no code impact).
- **`roast`** has follow-up: a newer roast may add subtests to whitelisted files.
  After re-vendoring, re-run every whitelisted file upstream changed and drop any
  that no longer pass to `TODO_roast/BLOCKERS.md`. Because of this churn, **roast
  updates go in their own PR** (see `docs/vendoring.md` §"Updating `roast`").
  Regenerate the roast × raku baseline if needed
  (`scripts/roast-raku-baseline.sh`).
- `roast/`, `raku-doc/`, `old-design-docs/` are **read-only** — never hand-edit;
  the only supported change is a re-vendor.

## 3. Bundled zef (`vendor/zef/`)

`vendor/zef/` is upstream [zef](https://github.com/ugexe/zef) (Artistic-2.0),
shipped with mutsu as `mzef`'s backend. It is **not** hand-edited. To bump it,
re-vendor per [`vendor/README.md`](../vendor/README.md) (an `rsync` recipe), then
run the mzef pipeline checks in `docs/mzef-install-pipeline.md`. zef is also the
compatibility north star: when `mzef` misbehaves, fix mutsu, not the vendored
zef.

## Definition of done for a maintenance pass

- `cargo build` + `cargo clippy -- -D warnings` + `make test` green locally.
- Each risky change is its own `chore(deps):` PR; CI (`make test` + `make roast`)
  is green before merge.
- `vendor.lock` committed alongside any re-vendor.
- Any whitelist removals recorded in `TODO_roast/BLOCKERS.md`.
