# A package-qualified name is built once, and a gate keeps it that way

`Pkg::thing` is a *derived* name: a function of the package and the thing,
both of which the caller already holds. Whether a name is qualified at all is
a function of its text, and the text is a compile-time constant. Yet the
interpreter kept asking both questions at run time, with
`format!("{pkg}::{name}")` and `name.contains("::")`.

This is the finding [`src/runtime/meta_ns.rs`](../../src/runtime/meta_ns.rs)
already records for `__mutsu_*` metadata keys — in a different namespace, and
measurably larger.

## What it costs

Profiling one `JSON::Fast` decode ([#8898](https://github.com/tokuhirom/mutsu/issues/8898),
callgrind, inclusive share of the decode):

| | share | |
|---|---:|---|
| `resolve_type_in_current_package` | 5.11% | 0.48% of the whole program in `format!` alone |
| `running_module_bareword` | 1.54% | +0.77% `format!` |
| `set_our_var` | 1.23% | |
| `resolve_type_name_for_owner` | 1.16% | +0.45% `format!` |

`<core::str::pattern::StrSearcher>::new` — the `"::"` searches — was
constructed **1,390,603 times** in a ten-decode run, and its top callers are
exactly those functions. `Symbol::intern` is 4.34% inclusive, much of it
re-interning names that had just been built.

`resolve_type_in_current_package`, the largest single function in that
profile, is the shape in miniature. It took the interpreter's package
`RwLock` and cloned the package name onto the heap; then, at every level of
the enclosing chain, it `format!`ed a `Pkg::name` candidate and `rsplit_once`d
the chain to get the next one. Every call rebuilt what every previous call
with the same package had already built.

## The constructor

[`src/qualified.rs`](../../src/qualified.rs) is what those sites should be
using. Symbols are global and append-only — an id, once assigned to a string,
is never reused or remapped — so every question below has one answer for the
life of the process, and every one is memoized:

| | |
|---|---|
| `qualified(pkg, name)` | `Pkg::name`, built once per pair |
| `package_ancestors(pkg)` | the enclosing chain, no allocation, each step a memo hit |
| `is_qualified(name)` | classified once per symbol, in a flag table indexed by id |
| `is_global_package(pkg)` | two id compares |

`resolve_type_in_current_package` and `resolve_type_name_for_owner` are
converted here. Measured against `main`, paired runs, both sides a fresh
`--profile profiling` build, one `from-json` ×10 each:

| | before | after |
|---|---:|---:|
| total instructions | 30,500,527,760 | 30,185,157,022 (**−1.03%**) |
| allocations | 19,300,740 | 17,809,300 (**−7.7%**) |
| `String::clone` allocations | 4,319,019 | 3,085,607 (−28.6%) |
| `malloc` / `free` | — | −7.7% / −7.7% |

Under the ~2% threshold where this box's wall clock can see anything, so no
wall-clock claim is made.

## The gate, which is the point

Two conversions is not the story. `check-magic-keys`'s own comment is:

> Each was fixed by memoizing the one key that profile happened to walk
> through, and the pattern grew back, because nothing stopped the next site
> being written.

It took 276 → 174 → 0 sites across three stages to finish that one, and it
only finished because a gate counted them the whole way. The same gate exists
now for qualified names: `make check-name-scans`, a shrinking ratchet over
three counts —

- **174** hand-built qualified names (`format!("{pkg}::{name}")`),
- **99** `"GLOBAL"` string compares,
- **348** `"::"` surgeries (`contains` / `split` / `rfind` / `has_double_colon`),

— which may go down and never up, wired into `make test` and CI, with a
`--self-test` so a regex that stopped matching cannot make the ratchet
silently report 0. The rule is in CLAUDE.md's Conventions next to `MetaNs`.

`src/parser/` and `src/compiler/` are exempt, deliberately: deciding what a
name *is* from its text is their job, and doing it there rather than once per
execution is the whole point of the rule.

It earned its keep before it landed. Rebasing this branch onto a `main` that
had moved ten commits made the gate fail on a `global-cmp` count of 100
against a baseline of 99 — and the new site was `trir_body_package`, written
by the ADR-0110 Stage 2 PR that merged an hour earlier. Converted, and the
count is back to 99. That is the failure mode this exists for: not a careless
site, a *reasonable* one, written by someone who had just spent a day in the
profile.
