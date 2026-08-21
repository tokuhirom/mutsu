# Battery: TOML parser — `Config::TOML`

**Slot:** TOML config-file parser/writer · **Selected:** `Config::TOML`
v0.1.3 (`auth<zef:raku-community-modules>`, Unlicense) + its dependency
`Crane` v0.1.2 (same auth, Unlicense) · **Kind:** Selected, not yet bundled
(blocked on mutsu core work) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard
gate) → dependency weight → maintainer stewardship → proven behaviour on
mutsu → API fit → "a small web blog can be written with the bundle alone"

Procedure: [selection-method.md](selection-method.md).

## Status: selected, not yet bundled

**Every credible candidate failed 0% under mutsu when first measured.** The
survey's real output is therefore a work list, exactly as
[selection-method.md §5](selection-method.md#5-expect-the-answer-to-be-fix-mutsu-first)
predicts. `Config::TOML` won the field on criteria, but shipping it means
fixing the mutsu bug it exposes first — see
[Why it currently fails on mutsu](#why-it-currently-fails-on-mutsu) below.

```raku
use Config::TOML;
my %config = from-toml('example.toml'.IO.slurp);   # not yet runnable on mutsu
```

## The field

Enumerated from the local REA + fez indices (14,834 dist names), filtered on
`toml` in name/description/tags.

| Candidate | Version | Released | License | auth | Runtime deps | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Config::TOML`** (+`Crane`) | 0.1.3 / 0.1.2 | 2024-11-12 | Unlicense / Unlicense | `zef:raku-community-modules` (both) | 1 (`Crane`, itself 0-dep) | 0 / 1 (`Crane`←`Config::TOML`) | **17/17** | **0/17** |
| `TOML` | 3 | 2021-05-01 | Artistic-2.0 | `zef:tony-o` | **0** | **7** (highest — `AI::Gator`, `Clu`, `Hey`, `LLM::DWIM`, `TooLoo`, `Zeco`, `App::SerializerPerf`) | 5/5 | **0/5** |
| `TOML::Thumb` | 0.2 | 2021-07-26 | MIT | `zef:JRaspass` | **0** | 0 | ~clean² | **0/2** |
| `Config::Parser::toml` | 1.0.4 | 2023-08-29 | **AGPL-3.0-only** | `zef:tyil` | 2 (`Config::TOML`, `Config`) | — | — | — |
| `Config::DataLang::Refine` | 0.7.6 | 2024-04-05 | Artistic-2.0 | — | 2 (`Config::TOML`, `JSON::Fast`) | — | — | — |

¹ Distributions in the local REA+fez indices whose `depends` names the
candidate, same methodology as [templates.md](templates.md).
² 194 assertions across `valid.t`/`invalid.t`; 18 are the upstream author's own
`# TODO 'not yet implemented'` markers (non-fatal under TAP), everything else
passes.

## Ruled out before measuring

- **`Config::Parser::toml`** — **AGPL-3.0-only** (an earlier 1.0.1-1.0.3 line
  was plain GPL-3.0). Copyleft; disqualified by the hard license gate
  ([BATTERIES.md §4](../../BATTERIES.md#4-license-policy)) before any other
  criterion was even checked.
- **`Config::DataLang::Refine`** — not a TOML parser itself, a refinement
  layer that sits *on top of* `Config::TOML`'s already-parsed output
  (whitespace/comment/type-coercion post-processing for config values). Out of
  scope for "the TOML parser slot"; worth a look later as a companion
  convenience once the parser itself is bundled.

## Why `TOML` (tony-o) was passed over despite the most dependents

`TOML` (`zef:tony-o`, v3, 2021-05-01) leads every ecosystem-standing metric —
Artistic-2.0, zero runtime deps, and by far the most dependents (7, vs. 0 for
every other candidate). It is healthy under raku (5/5, including a round-trip
encoder test). But its implementation (`TOML::NQP.rakumod`) is written
directly against **raw `nqp::` ops** (`use nqp;` + calls like
`nqp::ordat($t, $pos)`), a low-level coding style essentially unseen in
current-era Raku modules — consistent with the module's age (its last release
is now 5 years old). mutsu's parser cannot parse a `nqp::`-op-laden module at
all:

```
Failed to parse module 'TOML::NQP': Confused. parse error ...
```

Supporting arbitrary `nqp::` op syntax in general is not a bounded fix for
this one module — it is the same class of problem as `NativeCall`'s "needs
`QAST:from<NQP>`, MoarVM dispatch programs, and 61 missing `nqp::` ops" case
(`todo/deep/nativecall-cannot-be-vendored.md`), which CLAUDE.md already
documents as **measured and not currently retirable**. Per
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria), "proven behaviour
on mutsu" — "a candidate that already `use`s cleanly beats a 'better' one
that needs a multi-session core campaign just to load" — overrides the
dependents-count lead here. This is intentionally **excluded** from the
core-work queue below; revisit only if mutsu ever undertakes a general
NQP-op-support campaign for independent reasons.

## Why `TOML::Thumb` was runner-up, not the winner

`TOML::Thumb` (`zef:JRaspass`, MIT, 0 deps) is the best-scoped candidate by
"proven behaviour on mutsu": its entire suite is blocked by exactly **one**
narrow, well-understood core gap — see
`todo/tickets/dateish-role-not-registered-as-composable.md`. It lost to
`Config::TOML` on the newly-added **maintainer stewardship** criterion
(user decision, 2026-08-22 — see
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) and
[selection-method.md](selection-method.md)): `zef:JRaspass` is a single-author
dist with 0 dependents, versus `Config::TOML` (+ its own dependency `Crane`)
both being `auth<zef:raku-community-modules>` — the curated org already behind
most of this bundle's other Adopted entries (`HTTP::HPACK`,
`IO::Path::ChildSecure`, `JSON::JWT`, `Cro::*`, `DBIish`, ...). `Config::TOML`
also ships by far the most thorough upstream suite of the field (17 files vs.
2), giving the eventual battery-testsuite gate much more real coverage.
Keep `TOML::Thumb`'s ticket open regardless — its gap is general (any module
defining a `Dateish`-compatible type hits it) and worth fixing on its own
merits.

## Why it currently fails on mutsu

All 17 of `Config::TOML`'s upstream test files fail to load. The apparent
first symptom is a confusing, mis-line-numbered parse error:

```
===SORRY!=== Error while compiling t/special-cases/04-string-literal-keys.rakutest
expected statement: expected use statement or import statement or no statement or need statement or unit statement or ...
at t/special-cases/04-string-literal-keys.rakutest:28
```

— but line 28 does not exist meaningfully in that 59-line test file; it is
actually line 28 of `Config::TOML::Parser::Actions.rakumod`, reached via `use`
at compile time. **Root cause, fully bisected**:
`Config::TOML::Parser::Actions` and `::Grammar` declare dozens of
`method`/`token` **multi-dispatch variants named with a bare identifier
adverb** (`method string-basic-char:common (...)`, `token gap:spacer {...}`,
`token string:basic {...}` — 48 such methods in `Actions.rakumod` alone, plus
a matching set of `token`s in `Grammar.rakumod`), as opposed to the more
familiar `NAME:sym<literal>` spelling. mutsu's parser does not recognize this
bare-adverb spelling — filed as
`todo/tickets/token-method-bare-colon-adverb-name-not-supported.md`, with two
standalone minimal repros (one for `method`, one for `token` inside a
`grammar`) that reproduce independently of `Config::TOML` entirely. This is a
**general parser gap**, not TOML-specific — fixing it is a real interpreter
improvement per [BATTERIES.md §1](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)
rung 2 ("grow mutsu's core, not the library"), and is the entire blocker for
this slot.

## Provenance (for when the blocker clears)

Per [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution). Not yet
vendored — no `modules/` tree or `batteries.lock` entry exists for this slot
yet, per the "Selected, not yet bundled" convention in
[BATTERIES.md §7](../../BATTERIES.md#7-bundle-index).

| Module | Upstream | Pinned version | auth |
| --- | --- | --- | --- |
| `Config::TOML` | <https://github.com/raku-community-modules/Config-TOML> | v0.1.3 | `zef:raku-community-modules` |
| `Crane` (dependency) | <https://github.com/raku-community-modules/Crane> | v0.1.2 | `zef:raku-community-modules` |

When vendoring: `lib/` + `META6.json` + `LICENSE`/`UNLICENSE` + `README.md`
for both modules, excluding upstream `t/`, `run-tests`, `dist.ini`,
`Changes`-adjacent CI config, per the standard recipe.

## Security updates

Per [BATTERIES.md §6](../../BATTERIES.md#6-security-updates-and-independent-updatability),
once bundled, the vendored copy is the lowest-priority source; `mzef install
Config::TOML` (or `Crane`) shadows it without a mutsu release.

## License

**Unlicense** (public domain equivalent) for both `Config::TOML` and `Crane`
— declared in each `META6.json`, shipped as `UNLICENSE` in each checkout.
Permissive; passes [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)
cleanly (no provisional-exception caveat needed, unlike `Encode`).

## Next steps

1. Land `todo/tickets/token-method-bare-colon-adverb-name-not-supported.md`.
2. Re-run `Config::TOML` + `Crane`'s upstream suites (fetch per the REA
   `source-url`s above, or from a fresh `tmp/toml-survey/` per
   [selection-method.md](selection-method.md)'s procedure) and see how much
   of 0/17 clears.
3. If it reaches a workable state, vendor per
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), add the
   `batteries.lock` entries, run `scripts/battery-testsuite.sh --update`, and
   promote this record's status line + the [bundle index](../../BATTERIES.md#7-bundle-index)
   row from "Selected, not yet bundled" to "Working".
