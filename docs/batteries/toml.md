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
fixing the mutsu bugs it exposes first — see
[Why it currently fails on mutsu](#why-it-currently-fails-on-mutsu) below.

**Progress, 2026-08-22:** the original blocker (bare-identifier adverb
declaration names) is fixed —
[`news/2026-08/bare-adverb-declaration-names.md`](../../news/2026-08/bare-adverb-declaration-names.md).
`Config::TOML` now **loads**, its grammar parses documents correctly, and its
19 upstream files run to real per-assertion results instead of failing to
compile. It is still 0/19 at the file level; the work list below is what
remains.

**Re-measured 2026-08-26** (fresh REA fetch of both dists, each suite run from
its own directory against a release build): **`Config::TOML` 0/19, `Crane`
3/15**, against raku's 19/19 and 15/15 — unchanged. None of the fixes that
landed in the intervening days touch what `Crane`'s array-path descent needs.
`Crane`'s passing files are `at`, `flatten`, `test`; the dominant failure
elsewhere is `✗ Crane error: associative key does not exist`, plus a hard parse
error in `t/patch.rakutest` and a 90s timeout in
`Config::TOML`'s `t/grammar/03-inline-tables.rakutest`.

```raku
use Config::TOML;
my %config = from-toml('example.toml'.IO.slurp);   # not yet runnable on mutsu
```

## The field

Enumerated from the local REA + fez indices (14,834 dist names), filtered on
`toml` in name/description/tags.

| Candidate | Version | Released | License | auth | Runtime deps | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Config::TOML`** (+`Crane`) | 0.1.3 / 0.1.2 | 2024-11-12 | Unlicense / Unlicense | `zef:raku-community-modules` (both) | 1 (`Crane`, itself 0-dep) | 0 / 1 (`Crane`←`Config::TOML`) | **19/19** | **0/19**³ |
| `TOML` | 3 | 2021-05-01 | Artistic-2.0 | `zef:tony-o` | **0** | **7** (highest — `AI::Gator`, `Clu`, `Hey`, `LLM::DWIM`, `TooLoo`, `Zeco`, `App::SerializerPerf`) | 5/5 | **0/5** |
| `TOML::Thumb` | 0.2 | 2021-07-26 | MIT | `zef:JRaspass` | **0** | 0 | ~clean² | **0/2** |
| `Config::Parser::toml` | 1.0.4 | 2023-08-29 | **AGPL-3.0-only** | `zef:tyil` | 2 (`Config::TOML`, `Config`) | — | — | — |
| `Config::DataLang::Refine` | 0.7.6 | 2024-04-05 | Artistic-2.0 | — | 2 (`Config::TOML`, `JSON::Fast`) | — | — | — |

¹ Distributions in the local REA+fez indices whose `depends` names the
candidate, same methodology as [templates.md](templates.md).
² 194 assertions across `valid.t`/`invalid.t`; 18 are the upstream author's own
`# TODO 'not yet implemented'` markers (non-fatal under TAP), everything else
passes.
³ The first survey recorded "17"; a re-count on 2026-08-22 found 19
`.rakutest` files (`api` 1, `dumper` 1, `exceptions` 2, `grammar` 4,
`grammar-actions` 4, `special-cases` 7). `Crane`'s own suite is 15 files, of
which mutsu passes 3.

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
also ships by far the most thorough upstream suite of the field (19 files vs.
2), giving the eventual battery-testsuite gate much more real coverage.
Keep `TOML::Thumb`'s ticket open regardless — its gap is general (any module
defining a `Dateish`-compatible type hits it) and worth fixing on its own
merits.

## Why it currently fails on mutsu

### Fixed: bare-identifier adverb declaration names (2026-08-22)

Originally all of `Config::TOML`'s upstream test files failed to **load**, with
a confusing, mis-line-numbered parse error pointing at a line inside
`Config::TOML::Parser::Actions.rakumod` rather than the test file. Root cause:
`::Actions` and `::Grammar` declare dozens of `method`/`token` multi-dispatch
variants named with a **bare identifier adverb** (`method
string-basic-char:common (...)`, `token gap:spacer {...}` — 48 such methods in
`Actions.rakumod` alone), as opposed to the familiar `NAME:sym<literal>`
spelling, which was all mutsu recognized.

Fixed across the parser, proto-variant resolution and grammar-action dispatch,
along with two gaps found on the way (`my Array[Str:D] @k` mis-flagged as an
invalid type smiley; an object hash never binding to a typed `%h` parameter) —
see [`news/2026-08/bare-adverb-declaration-names.md`](../../news/2026-08/bare-adverb-declaration-names.md).
This was a **general parser gap**, not TOML-specific, so fixing it was rung-2
work per [BATTERIES.md §1](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is).

### Remaining work list

Measured 2026-08-22 against a release build, running each suite from its own
dist directory:

| # | Gap | Filed as | Blocks |
| --- | --- | --- | --- |
| 1 | ~~**`is rw` routines do not return an lvalue.**~~ **FIXED for the hash half** — ADR-0059, `news/2026-08/is-rw-routines-return-a-container.md`. An `is rw` routine now returns a container and the assignment writes through it, so `Crane.set(%h, :path["a","b"], :value(1))` produces `{:a(${:b(1)})}` like raku (Crane subtests 263→280 ok). Residue, all Crane-side: (a) ~~the deferred vivification token is hash-only~~ **FIXED** — `news/2026-08/deferred-vivification-path-steps-are-typed.md`; the token's path steps are typed, so `Crane.set(%h, :path["a", 0], :value(1))` now yields `{:a($[1])}` like raku (Crane subtests 280→283 ok, `t/in` 9→12). Crane's own residue is instead `.add`/`.copy`'s deep clone ("Original container is unchanged"); (b) `X::Crane::PositionalIndexInvalid` is not raised by `Crane::Utils`' classifier multis; (c) WhateverCode (`*-0`) indices do not survive the descent. | (a) fixed; (b)/(c) un-bisected | `Crane` 12/15 (array paths), and every `Config::TOML` file that builds a result |
| 2 | ~~**A `\|\|` alternation runs the losing branch's code block.**~~ **FIXED** — `news/2026-08/ordered-alternation-loser-branch-code-block.md`. `\n`, `\"` and `\\` now parse. Residue: the 8-hex `\UXXXXXXXX` form still reports "bad string escape sequence 「U」" although `token escape:sym<U> { <sym> <hex> ** 8 }` matches in isolation — a narrower, un-bisected candidate-selection problem. | — | `grammar/04`, `grammar-actions/04` |
| 3 | ~~**`push(@a, 1, \|@rest)` in sink context** resolves to "Unknown call: push".~~ **FIXED** — `news/2026-08/listop-slip-arg-sink-context.md`. | — | — |
| 4 | Assorted per-assertion failures in `grammar/01-02`, `grammar-actions/01-02`, `exceptions/01-02`, `dumper/01` (the last one a `multi to-toml` candidate-selection mismatch: `Calling to-toml(Str) will never work with declared signature (Associative:D $container, %opts)`). Not yet bisected. | — | the rest |

Item 1 is a general interpreter gap with standalone repros that do not mention
TOML; so were items 2 and 3.

**Re-measured after items 2 and 3 landed (2026-08-22):** still 0/19 at the file
level, but the failure *shape* moved. `Unknown call: push` is gone, and the
dominant remaining error across seven files is now `Sorry, table contains
duplicate keys` — which is item 1 surfacing from a different direction: with
`Crane.set` silently doing nothing and `Crane.exists` therefore answering from
an empty container, `Config::TOML`'s own duplicate-key guard fires on every
document. Item 1 really is the whole gate.

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

1. **Land the residue of item 1** —
   `todo/deep/deferred-vivification-token-is-hash-only.md`. The `is rw` lvalue
   return itself landed (ADR-0059), so `Crane.set` works for hash paths; what
   remains is the array twin of the same mechanism.
2. ~~Land items 2 and 3~~ — both landed 2026-08-22. Item 2 left one residue
   (the 8-hex `\U` escape) worth bisecting alongside item 4.
3. Re-run `Config::TOML` + `Crane`'s upstream suites (fetch per the REA
   `source-url`s above, or from a fresh `tmp/toml-survey/` per
   [selection-method.md](selection-method.md)'s procedure) and bisect whatever
   of item 4 is left.
4. If it reaches a workable state, vendor per
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), add the
   `batteries.lock` entries, run `scripts/battery-testsuite.sh --update`, and
   promote this record's status line + the [bundle index](../../BATTERIES.md#7-bundle-index)
   row from "Selected, not yet bundled" to "Working".
