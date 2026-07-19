# Ecosystem survey: how much of the Raku ecosystem needs an NQP/compiler-guts layer?

**Date:** 2026-07-19 · **Reproduce:** `scripts/ecosystem-guts-survey.py [SAMPLE_SIZE] [SEED]`

## Question

mutsu deliberately does **not** implement a MoarVM/NQP layer (no `nqp::` runtime,
no `QAST`, no slang/grammar mutation, no `Metamodel::Primitives`). Before
investing in one, we need to know: **how much of the ecosystem is actually
blocked by needing that layer**, versus reachable by improving pure-Raku
compatibility?

This came up while scoping **Test::Async** (PLAN.md §1 B2b), which turned out to
need the full guts stack (custom `package_declarator` slang tokens via
`define_slang`, `QAST`, the `$*W` World, `NQPHLL`) — not merely "custom HOW
inheritance". The natural follow-up: is Test::Async a lone outlier, or the tip of
a large blocked-by-guts iceberg?

## Method

1. Read the local fez ecosystem index (`~/.zef/store/fez/fez.json`, the same
   index `mzef`/zef uses — 7660 entries, **1573 unique distributions**).
2. Keep the latest version per dist; draw a **fixed-seed random sample**.
3. Download each dist tarball from the fez CDN and scan its Raku sources
   (`.rakumod/.pm6/.raku/.rakutest/.t`), placing each dist in exactly **one**
   bucket by decreasing severity:

| Bucket | Signal | Reachability on mutsu |
| --- | --- | --- |
| `deep_guts` | `QAST`, `Metamodel::Primitives`, `NQPHLL`, `:from<NQP>`, `EXPORTHOW`, `define_slang`, `package_declarator:sym`, `experimental :macros`, `Slang::` | **Blocked** — needs the guts layer mutsu doesn't have (Test::Async class) |
| `nativecall` | `use NativeCall`, `is native`, `:from<C>` | Needs the C FFI layer, which mutsu **has as an MVP** — partially reachable, **not** NQP-blocked |
| `nqp_ops_only` | `use nqp` / `nqp::` (none of the deep signals) | A spectrum — many `nqp::` ops are simple and stubbable / perf-only |
| `pure_raku` | none of the above | **Not guts-blocked** |

## Results

Two independent random samples, consistent:

| Bucket | n=150 (seed 42) | n=60 (seed 20260719) |
| --- | --- | --- |
| `pure_raku` | **121 (81%)** | 41 (68%) |
| `nativecall` | 14 (9%) | 11 (18%) |
| `nqp_ops_only` | 8 (5%) | 5 (8%) |
| **`deep_guts`** | **7 (5%)** | **3 (5%)** |

`deep_guts` hits (n=150): `Slang::Roman`, `Slang::NumberBase`, `Grammar::Debugger`
(custom slangs), `Data::Record` (macros), `App::Crag`, `Kind::Subset::Parametric`,
`Backtrace::Files` — the same "compiler-guts" class as Test::Async.

Reverse-dependency check for Test::Async specifically: **8 of 1573 dists** depend
on it, **all as `test-depends` only** (needed to run those dists' own test
suites, never at runtime/install), and concentrated in one author's (vrurg)
ecosystem.

## Interpretation

- **The genuinely guts-blocked tail is small: ~5%** (at most ~10% if you count
  every `nqp::`-touching dist as blocked, which overcounts — most such uses are
  stubbable or perf-only).
- **~81% of the ecosystem is pure Raku** and reachable purely by widening
  mutsu's pure-Raku compatibility.
- **~9% need NativeCall**, a *separate* native layer mutsu already has an MVP of
  (real SQLite CRUD round-trips) — reachable by completing FFI, not by NQP.

## Strategic conclusion

Building a full NQP / QAST / slang layer would be an enormous effort to unlock
only the ~5% `deep_guts` tail — the same poor cost/benefit that led us to defer
Test::Async. The high-leverage work is the **~90%** that is pure Raku or
NativeCall:

- **Do** PLAN.md §1 **B1** (finalize + document the batteries bundle) and **B4**
  (close pure-Raku compatibility gaps, surfaced by running real dists) — these
  raise the reachable ceiling for the 81% majority.
- **Do** keep improving **NativeCall** (the 9%), which needs no NQP layer.
- **Defer** a general NQP/slang subsystem. When a specific high-value
  `deep_guts` dist is wanted, prefer a **narrow per-feature shim** (e.g. treat a
  custom declarator as a built-in parse rule) over a general guts emulator.

## Caveats

- fez ecosystem only (not the older github/CPAN Raku ecosystems).
- A **signal scan, not execution**: `pure_raku` means "not guts-blocked", **not**
  "runs on mutsu today" (a dist may still hit an unimplemented pure-Raku
  feature). Conversely a `deep_guts` dist may use its guts feature only in a
  cold path.
- Sample ≈ 10% of the fez index; the 5% `deep_guts` figure has a sampling error
  of a couple of points. Re-run `scripts/ecosystem-guts-survey.py 300` for a
  tighter estimate.
