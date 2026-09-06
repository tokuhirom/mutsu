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
| **`Template::Mustache`** | 1.2.6 | 2026-01-12 | Artistic-2.0 | **0** | **11** | 11/13² | **13/13** |
| `Template6` | 0.16.0 | 2026-02-04³ | Artistic-2.0 | **0** | 7 | **12/12** | **10/12** ⬆ |
| `Template::Jinja2` | 0.2.0 | 2026-04-29 | Artistic-2.0 | 1 (`JSON::Fast`, native) | 2 | 22/23 | **3/23** ⬆ |
| `Template::Mojo` | 0.2.2 | 2023-07-31 | MIT | **0** | 3 | **5/5** | **4/5** |
| `Template::Nest::Fast` | 0.3.0 | 2024-11-18 | ISC | **0** | 0 | **10/10** | **0/10** |
| `SP6` | 0.2.1 | 2021-09-04 | Apache-2.0 | **0** | 0 | 10/11 | **10/11** |
| `Template::Classic` | 0.0.3 | 2020-04-11 | BSD-3-Clause | **0** | 1 | **1/1** | 0/1 |
| `Template::HAML` | 0.9.5 | 2026-06-27 | Artistic-2.0 | **0** | 2 | 82/83 | 39/83⁴ |
| `Template::Protone` | 0.1.4 | 2021-01-20 | Artistic-2.0 | **0** | 0 | *ships no tests* | *ships no tests* |
| `ERK` | 1.1.4 | 2025-11-14 | Artistic-2.0 | **0** | 1 | *ships no tests* | *ships no tests* |

The mutsu column was **re-measured in full on 2026-09-06** (debug build). Do not
quote a row without re-running the survey: on that re-run four of the eight rows
had moved since the last measurement, all from unrelated work.

⬆ `Template6` went **0/12 → 10/12** on 2026-09-06. Its long-standing
"unreduced, `Use of Nil in string context`" state turned out to hide **four**
independent general interpreter bugs, none of them the warning: a `split(/…/, :v)`
separator `Match` carried no named captures, `.subst(…, :nth(2..*))` aborted the
process with a Rust `capacity overflow`, an attribute default's closure was
stamped with the *constructing* class (so it could not see its own file's subs),
and an assignment to `$_` inside a nested block was discarded on block exit. Pins:
`t/split-regex-separator-captures.t`, `t/subst-nth-range.t`,
`t/attr-default-closure-package.t`, `t/topic-assign-in-nested-block.t`. Write-up:
`news/2026-09/template6-zero-to-ten-of-twelve.md`.

`Template::Mustache` itself went **1/13 → 11/13** on 2026-07-25 when the single
interpreter bug behind it was fixed: a hyper method call (`@objs>>.made`) did not
flatten a `Slip` returned by the method, so the parse tree came out with each
hunk's `Slip` nested — and `.flat` then decomposed the `Hash` inside it into
Pairs. Pin: `t/hyper-method-slip-result.t`. The last two files followed the same
day (**11/13 → 13/13**) from three more general fixes: a subscript assignment
through a `$`-sigil attribute (`$!h<k> = 1`) reaching the instance, a `for` block
no longer leaking its topic into the enclosing `$_`, and text-mode file reads
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

As of the 2026-09-06 re-measurement. None of these are module rot — raku runs
them all.

| Candidate | Symptom |
| --- | --- |
| `Template::Mustache` | none; 13/13 |
| `Template6` | 10/12. `02-for`: an `@` argument's mutation is lost on the *second* call through a slurpy relay (`todo/tickets/array-arg-mutation-lost-on-the-second-call-through-a-slurpy-relay.md`). `05-includes`: `[% INCLUDE "x" name = "World" %]` renders `name` instead of `World` (`todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md`) |
| `Template::Jinja2` | loads now (3/23); the rest are ordinary per-feature failures. Its last load blocker — `Renderer.rakumod:114`'s `when If {` read as a call — was fixed 2026-09-06 |
| `Template::Mojo` | `00-basic` only; `todo/tickets/template-mojo-residual-failures.md` |
| `Template::Nest::Fast` | `with $f ~~ m:g/…/ -> @m` binds `@m` to a one-element list *containing* the match list, so `$m[0].from` is Nil. `with ("a<!--x-->b<!--yy-->c" ~~ m:g/('<!--') \s* (\w+) \s* ('-->')/) -> @m { say @m.elems }` gives 2 under raku, 1 under mutsu |
| `Template::Classic` | `Unterminated <%` from its own grammar — the `$<part> = <rule>` capture-assignment form inside a `||` chain does not match |
| `SP6` | at parity with raku (both 10/11, same file) |

The old "`Use of Nil in string context`" entries are gone: that line was a
*warning* in both implementations and never the diagnosis, exactly as
`todo/deep/template-engines-blocked-on-mutsu.md` warned. Every row that was
reduced turned out to be something else entirely.

Confirmed and separately filed so far (all three of the older entries here are
now **fixed**; they are kept because each was a general bug found through this
survey):

- ~~`todo/tickets/q-heredoc-interpolates-qq-escape.md`~~ — `Q:to/…/` wrongly
  honoured `\qq[…]`; raku leaves it literal. Fixed 2026-07-26; it was **not**
  the `Template6` blocker.
- ~~`todo/tickets/regex-brace-paren-inside-char-class-swallows-rest-of-pattern.md`~~
  — a literal `{` / `(` inside a `<[...]>` char class made
  `scan_angle_assertion_body()` miss the assertion's closing `>`. Fixed;
  `news/2026-08/regex-char-class-literal-brace-paren.md`. Took
  `Template::Jinja2`'s `01-lexer` from 0/15 to 15/15.
- ~~`todo/tickets/qualified-private-method-call-uses-short-owner-name.md`~~ — a
  qualified private call `$obj!Renderer::meth` inside
  `module Template::Jinja2::Renderer` compared the owner name as written against
  the fully-qualified caller class. Fixed;
  `news/2026-08/private-method-qualified-short-owner-in-module.md`. It was
  **not** the last Jinja2 load blocker — an imported-`is export`ed-type parse
  bug was stacked behind it, fixed 2026-09-06 (pin
  `t/when-imported-exported-type.t`).
- Open, from the 2026-09-06 re-measurement:
  `todo/tickets/array-arg-mutation-lost-on-the-second-call-through-a-slurpy-relay.md`,
  `todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md`,
  `todo/tickets/trailing-comma-in-attribute-default-drops-the-declaration.md`,
  `todo/tickets/template-mojo-residual-failures.md`,
  `todo/tickets/grammar-heavy-module-load-slower-than-raku.md`.
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
- **`Template::Jinja2` deserves a second look once it loads** — it is the newest
  of the field (2026-04-29) and 22/23 under raku. Both blockers named in the
  2026-08-19 reduction are fixed and `01-lexer` passes, but a third one (a
  `when TYPENAME {` parse bug) still kills the remaining 22 files at load, so it
  is still the cheapest of the field to unblock by file count. Its ecosystem
  standing is weak (2 dependents, both by the same author).
- **`Template::Protone` and `ERK` were never in contention**: they ship no tests
  at all, so there is nothing to gate at release time — a structural problem for
  a battery whose whole verification story is `scripts/battery-testsuite.sh`.

The deciding move was that Mustache's failure turned out to be **one interpreter
bug**, not a pile of them, so fixing it both unblocked the strongest candidate
and improved mutsu generally. The other engines' blockers stay on the work list
(`todo/deep/template-engines-blocked-on-mutsu.md`). `Template6` was fixed for
exactly that reason — so the slot has a real second option rather than a single
viable choice — and now runs 10 of its 12 files
(`news/2026-09/template6-zero-to-ten-of-twelve.md`).

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
