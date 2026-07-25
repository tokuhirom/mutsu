# Battery: template engine — `Template::Mustache`

**Slot:** Template engine · **Chosen:** `Template::Mustache`
(`auth<zef:raku-community-modules>`, v1.2.6, Artistic-2.0) · **Kind:** Adopted
(community module, vendored as-is) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard gate)
→ dependency weight → proven behaviour on mutsu → API fit → "a small web blog can
be written with the bundle alone"

The procedure that produced the table below is written up separately and is meant
to be reused for every future slot: [selection-method.md](selection-method.md).
It exists because this slot's shortlist was carried in `PLAN.md` as prose
("Template::Mustache, 91/92 specs") that turned out to be **stale** — re-measured
on 2026-07-25, that engine passed **1 of its 13** upstream test files under
mutsu.

## Status: bundled

`Template::Mustache` ships at `modules/Template-Mustache/` and resolves with
**zero config**:

```raku
use Template::Mustache;
say Template::Mustache.render('Hello {{name}}!', { name => 'World' });   # Hello World!
```

It won on every axis the criteria rank: Artistic-2.0, **zero runtime
dependencies**, the most-depended-on engine in the ecosystem (11 dependents,
including `Bailador`, `Documentable` and `Pod::To::HTML`), maintained under
`raku-community-modules` like most of what mutsu already bundles, and
logic-free — which is the safer default for a blog, where the program supplies
the logic. It also implements a **cross-language format**, so a template written
against it is not mutsu-specific knowledge.

**All 13 upstream files pass**, including both official mustache spec suites
(`91-specs` from strings and `92-specs-file` from files, 10/10 each). The two
that were still failing when the module was first bundled were fixed on
2026-07-25 and are pinned in `batteries-whitelist.txt` like the rest, so a
regression in any of them fails a release.

## The field it was chosen from

Every serious candidate was **healthy under raku and broken under mutsu** when
first measured, so the choice was not really between modules — it was about which
mutsu bugs to fix. Numbers are whole upstream test files fully passing
(`prove`-style: a TAP plan, every planned test `ok`, no `not ok`), run against a
plain checkout of the dist with `-I lib`.

| Candidate | Version | Released | License | Runtime deps | Dependents¹ | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **`Template::Mustache`** | 1.2.6 | 2026-01-12 | Artistic-2.0 | **0** | **11** | 11/13² | **13/13** ⬆ |
| `Template6` | 0.16.0 | 2026-02-04³ | Artistic-2.0 | **0** | 7 | **12/12** | **0/12** |
| `Template::Jinja2` | 0.2.0 | 2026-04-29 | Artistic-2.0 | 1 (`JSON::Fast`, native) | 2 | 22/23 | **0/23** |
| `Template::Mojo` | 0.2.2 | 2023-07-31 | MIT | **0** | 3 | **5/5** | **0/5** |
| `Template::Nest::Fast` | 0.3.0 | 2024-11-18 | ISC | **0** | 0 | **10/10** | **0/10** |
| `SP6` | 0.2.1 | 2021-09-04 | Apache-2.0 | **0** | 0 | 10/11 | 6/11 |
| `Template::Classic` | 0.0.3 | 2020-04-11 | BSD-3-Clause | **0** | 1 | **1/1** | 0/1 |
| `Template::HAML` | 0.9.5 | 2026-06-27 | Artistic-2.0 | **0** | 2 | 82/83 | 14/83⁴ |
| `Template::Protone` | 0.1.4 | 2021-01-20 | Artistic-2.0 | **0** | 0 | *ships no tests* | *ships no tests* |
| `ERK` | 1.1.4 | 2025-11-14 | Artistic-2.0 | **0** | 1 | *ships no tests* | *ships no tests* |

⬆ `Template::Mustache` went **1/13 → 11/13** on 2026-07-25 when the single
interpreter bug behind it was fixed: a hyper method call (`@objs>>.made`) did not
flatten a `Slip` returned by the method, so the parse tree came out with each
hunk's `Slip` nested — and `.flat` then decomposed the `Hash` inside it into
Pairs. The whole official mustache spec suite (`91-specs`, 10/10) passes now.
Pin: `t/hyper-method-slip-result.t`. The last two files followed the same day
(**11/13 → 13/13**) from three more general fixes: a subscript assignment through
a `$`-sigil attribute (`$!h<k> = 1`) reaching the instance, a `for` block no
longer leaking its topic into the enclosing `$_`, and text-mode file reads
decoding CRLF to LF. Pins: `t/attr-subscript-assignment.t`,
`t/for-topic-restore.t`, `t/io-crlf-translation.t`.

¹ Distributions in the ecosystem index that declare a dependency on it —
computed over the 2506 distinct dist names in the local REA + fez indices
(`~/.zef/store/{rea,fez}/*.json`), the same data `mzef` uses.
² The two `91/92-specs` files need `JSON::Fast` from the ecosystem, which is not
installed for the raku baseline; they are a harness gap, not a raku failure.
³ REA's newest is 0.15.0 (2026-02-04); fez carries 0.16.0, which is what was
measured.
⁴ `Template::HAML` is also **slower under mutsu than under raku** — a separate
finding from the failures. In a *release* build the gap is ~2–3× and looks like a
fixed module-load cost (`use Template::HAML` alone: mutsu 0.79s vs raku 0.35s),
not a per-test blow-up. (A debug build shows ~20×, which is debug overhead, not
the real figure — measure release.) See
`todo/tickets/grammar-heavy-module-load-slower-than-raku.md`.

### First observed failure under mutsu

Enough to start root-causing; none of these are module rot, since raku runs them.

| Candidate | Symptom |
| --- | --- |
| `Template::Mustache` | `Use of Nil in string context` from the `TOP` grammar action (`lib/Template/Mustache.rakumod:136`), reached via `parse-template` |
| `Template6` | same warning, from `Parser.compile` — a `q:to/RAKU/` heredoc whose `\qq[$safe-delimiter]` / `\qq[$segment]` come out empty |
| `Template::Jinja2` | `Cannot call private method without permission` at `Renderer.rakumod` load time — **22 of 23 files die on it**, the single biggest lever in the table |
| `Template::Mojo` | `No such method 'characters' for invocant of type 'Match'` — the grammar's `token characters` is not being resolved as a subrule (`.characters` is not a raku method either, so mutsu is falling back to method dispatch where it should be a named capture) |
| `Template::Nest::Fast` | `Use of Nil in string context` |
| `Template::Classic` | `X::Method::NotFound: Unknown method value dispatch (fallback dispatch)` |
| `SP6` | 5 files fail; `Use of uninitialized value element of type Any in string context` |

Note the warning text is a *warning* in both implementations and is not itself
fatal (verified) — it is simply the first non-TAP line the harness captured, so
treat it as a pointer, not the diagnosis.

Confirmed and separately filed so far:

- `todo/tickets/q-heredoc-interpolates-qq-escape.md` — `Q:to/…/` wrongly honours
  `\qq[…]`; raku leaves it literal. Found while reducing the `Template6` failure.
- `todo/deep/template-engines-blocked-on-mutsu.md` — this matrix as a work item.

## How the field was surveyed

The ecosystem was enumerated from the **local REA + fez indices** rather than by
guesswork: 2506 dists, filtered on name/description/tags for templating, then
each candidate's tarball fetched straight from the REA archive at its pinned
version and its own suite run under both `raku` and `target/debug/mutsu`
(`tmp/tmpl-survey.sh`). Reverse-dependency counts come from the same indices.

## Ruled out before measuring

- **`Cro::WebApp`** (0.10.1, Artistic-2.0, 9 dependents) — the modern de-facto
  choice *inside the Cro stack*, and its templates are good. Rejected for the
  same reason `Cro::HTTP::Client` was rejected for the
  [client slot](http-client.md): it depends on `Cro::HTTP`, `Log::Timeline` and
  `OO::Monitors`, i.e. bundling it means bundling Cro. Reconsider only if mutsu
  ever adopts Cro wholesale.
- **`HTML::Template`** (0.0.1, 3 dependents) and **`Text::Template`** (1.0.9) —
  **no license declared anywhere**. That is the hard gate in
  [§4](../../BATTERIES.md#4-license-policy); cf. the `Encode` situation, which we
  are already carrying provisionally and do not want to repeat.
- **`Stache`** (0.2.0, 2020) — depends on `YAMLish`, unmaintained, 0 dependents.
- **`Hinges`** (2017, no version, no license), **`Plosurin`** (0.02, 2018) —
  abandoned.
- **`Template::Anti`** (0.5.2, 2018-11-12, 0 dependents) — an interesting design
  (templates are plain HTML, logic attaches via selectors) but it depends on
  `DOM::Tiny` and has been dormant for 7 years.

## Why `Template::Mustache`, and what the runners-up were

- **`Template::Mustache` and `Template6` were the two ecosystem-credible
  choices.** Mustache leads on dependents (11 — `Bailador`, `Documentable`,
  `Pod::To::HTML`, `Hematite`, `Hiker`, …), is maintained under
  `raku-community-modules` like most of what mutsu already bundles, is
  Artistic-2.0 with **zero runtime dependencies**, and is a cross-language format
  a blog author may already know. `Template6` is the TT2-style alternative:
  also zero-dep, also Artistic-2.0, 7 dependents (`Uzu`, `TooLoo`, `Pekyll`), and
  **12/12 under raku** — a cleaner baseline than Mustache's.
- **Logic-less vs logic-ful is the real API-fit question.** Mustache is
  deliberately logic-free; `Template6`/`SP6`/`Template::Mojo`/`ERK`/
  `Template::Classic` embed Raku code. For "a small web blog", logic-free plus
  the host program's own code is the safer default, and it is what most of the
  ecosystem picked.
- **`Template::Jinja2` deserves a second look after its blocker is fixed** — it
  is the newest of the field (2026-04-29), 22/23 under raku, and its single mutsu
  error kills 22 files at once, so it may be the cheapest of all to unblock. Its
  ecosystem standing is weak (2 dependents, both by the same author).
- **`Template::Protone` and `ERK` were never in contention**: they ship no tests
  at all, so there is nothing to gate at release time — a structural problem for
  a battery whose whole verification story is `scripts/battery-testsuite.sh`.

The deciding move was that Mustache's failure turned out to be **one interpreter
bug**, not a pile of them, so fixing it both unblocked the strongest candidate
and improved mutsu generally. The other engines' blockers stay on the work list
(`todo/deep/template-engines-blocked-on-mutsu.md`); `Template6` in particular is
worth fixing so the slot has a real second option rather than a single viable
choice.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Template::Mustache` | <https://github.com/raku-community-modules/Template-Mustache> | v1.2.6 | `27f3e862` (2026-01-12) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`.
Upstream `t/`, `xt/`, `doc/`, `logotype/`, `dist.ini` and `.precomp` artifacts are
excluded — the release gate fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Template-Mustache/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/Template-Mustache/
# then bump the commit in batteries.lock, re-run the gate, refresh the manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use Template::Mustache; say Template::Mustache.render(q<{{x}}!>, { x => 42 })'   # 42!
```

## Security updates

Per [BATTERIES.md §6](../../BATTERIES.md#6-security-updates-and-independent-updatability)
the bundled copy is the lowest-priority source, so `mzef install
Template::Mustache` shadows it without a mutsu release.

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`. Vendored
verbatim with its `LICENSE` / `META6.json` / `README` preserved for attribution,
source unmodified (per [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
