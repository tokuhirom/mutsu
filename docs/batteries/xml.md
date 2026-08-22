# Battery survey: XML parsing and generation

**Slot:** XML parse + generate (DOM-style tree building, serialization back to an XML
string) · **Status:** **no candidate bundled — survey only** (2026-08-22) ·
**Yardstick:** [BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license
(hard gate) → dependency weight → proven behaviour on mutsu → API fit ·
**Procedure:** [selection-method.md](selection-method.md)

Flagged as gap #3 in
[python-stdlib-comparison.md](python-stdlib-comparison.md)'s "Structured Markup
Processing Tools" section: Python's `xml.etree.ElementTree`/`xml.dom`/`xml.sax` have no
mutsu equivalent. This is the first pass at the slot. **Selection criterion (user
decision, matching the CSV survey): the candidate must support both parsing and
generating XML** (build a tree in memory, serialize it back to an XML string), not
reading alone — the same rule that disqualified `CSV::Parser` in [csv.md](csv.md).

## A note on the no-native-dependency rule

Like the compression slot ([compression.md](compression.md)), a native/C dependency is
**not** an automatic disqualifier here (same user decision, restated for this slot): a
NativeCall binding to `libxml2` is architecturally the same shape as the already-bundled
`OpenSSL`/`DBIish`. `libxml2` (both the runtime `.so` and the `/usr/include/libxml2` dev
headers) is present on this survey machine (`ldconfig -p | grep -i libxml` shows
`libxml2.so.2`/`libxml2.so`; `xml2-config --version` reports `2.9.14`), so a native
candidate's build step was actually exercised, not just assumed. That said, a healthy
pure-Raku candidate is preferred when one exists, since it avoids the system-library
runtime dependency entirely — and this slot turned out to have exactly that.

## The field

Enumerated from `~/.zef/store/{rea,fez}/*.json` by filtering name/description/tags on
`xml|xhtml|dom|sax|xpath|xslt` (192 raw hits in `rea.json`). Most hits were noise:
general tools that merely touch XML as one input/output format among several
(`Audio::Hydrogen`, `Cmark`, `Gnome::Gtk3::Glade`, `Map::Mapnik`, `PDF::Extract`,
`PDF::Tags::Reader`, `JSON::Path`, `Data::DPath`, `Qwiratry::Format::XML`), CSS-tooling
that uses `xml`/`xpath` only as dependency tags (`CSS`, `CSS::Selector::To::XPath`,
`CSS::TagSet`), or HTML-focused converters (`HTML::Parser::XML`, `XML::Entity::HTML`).
`fez.json` carried only stale versions of the real candidates below (its `LibXML` entry
is 0.9.9 against `rea.json`'s current 0.11.3), so `rea.json` is the field of record here,
same as the CSV and compression surveys.

The real candidates, after excluding the noise above:

| Candidate | Version | Released | License | Runtime deps | Read+generate? | Native/C dep? | Dependents¹ | GitHub | raku | **mutsu** |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`XML`** | 0.3.6 | 2025-02-21 | Artistic-2.0² | none | ✅ both — full DOM tree, `.Str`/`.emit` serialization | none (pure Raku) | **45** | [raku-community-modules/XML](https://github.com/raku-community-modules/XML) — ★31, last push 2025-02-26, not archived | **15/15** (149 tests) | **1/15** ❌ — see below |
| **`LibXML`** | 0.11.3 | 2026-06-10 | Artistic-2.0² | `File::Temp`, `Method::Also`, `W3C::DOM`, `XML` (+build-time `LibraryMake`) | ✅ both — full DOM/SAX/XPath, `.Str` serialization | **libxml2 (native)** | **7** | [libxml-raku/LibXML-raku](https://github.com/libxml-raku/LibXML-raku) — ★13, **last push 2026-06-10** (most recent in this survey), not archived | **70/70** (723 tests) | **0/70** ❌ — see below |
| `XML::Writer` | * (2017) | 2017-05-26 | Artistic-2.0² | none | ❌ generate-only — **disqualified** | none | 0 | [masak/xml-writer](https://github.com/masak/xml-writer) — ★8, last push 2017-05-26, not archived | **2/2** (11 tests) | **2/2** ✅ (11/11) |
| `XML::Fast` | 0.0.3 | 2023-09-12 | Artistic-2.0 | `LibXML` | ❌ read-only (XML→Hash deserializer) — **disqualified** | libxml2 (via `LibXML`) | 0 | [jonathanstowe/XML-Fast](https://github.com/jonathanstowe/XML-Fast) — not checked³ | **3/3** (5 tests) | not measured⁴ |
| `XML::XPath` | 0.9.3 | 2019-04-11 | Artistic-2.0 | `XML` | extension layer, not a distinct reader/writer | none | 0 | [ufobat/XML-XPath](https://github.com/ufobat/XML-XPath) — ★0, last push 2017-07-30, not archived | not measured⁵ | not measured⁵ |
| `DOM::Tiny` | 0.5.2 | 2019-01-06 | Artistic-2.0 (README only)⁶ | none | ✅ both, but HTML-first/relaxed parser, not a strict XML DOM | none | 0 | [zostay/raku-DOM-Tiny](https://github.com/zostay/raku-DOM-Tiny) — ★13, last push 2026-07-08, **archived: true** | not measured⁷ | not measured⁷ |
| `W3C::DOM` | 0.0.3 | 2022-06-29 | Artistic-2.0 | none | abstract interface roles only, not an implementation | none | 0 | not resolved | n/a | n/a |
| `LibXML::Writer` | 0.0.5 | 2026-02-07 | Artistic-2.0 | `LibXML` | extension of `LibXML` (streaming writer) | libxml2 (via `LibXML`) | 0 | [libxml-raku/LibXML-Writer-raku](https://github.com/libxml-raku/LibXML-Writer-raku) — ★0, last push 2026-03-29, not archived | not measured⁸ | not measured⁸ |
| `LibXSLT` | 0.1.8 | 2026-02-21 | Artistic-2.0 | `LibXML` | XSLT transform layer, not a parser/writer itself | libxml2+libxslt (via `LibXML`) | 0 | [libxml-raku/LibXSLT-raku](https://github.com/libxml-raku/LibXSLT-raku) — not checked³ | not measured⁸ | not measured⁸ |

¹ Distributions in the local REA index whose `depends` names the candidate — same method
as [csv.md](csv.md)/[compression.md](compression.md). `XML`'s 45 dependents (versus
`LibXML`'s 7) is the largest dependents count seen across every battery survey run to
date (CSV's leader had 0; compression's leader had 4) — a strong, independent signal
that `XML` is the ecosystem's de facto standard XML library, matching the task's own
expectation going in.
² Cross-checked: a shipped `LICENSE` file (Artistic License 2.0, Perl Foundation
copyright header) matches the `META6.json` `license` field for `XML`, `LibXML`, and
`XML::Writer` — no license-gate concerns for any of the three measured candidates.
³ Stars/last-push not resolved for this candidate — deprioritized once it was ruled out
on the read+generate criterion (`XML::Fast`) or as a `LibXML` extension layer
(`LibXSLT`), so a `gh repo view` call was not worth spending here.
⁴ Not measured under mutsu: `XML::Fast` depends on `LibXML`, which is already 0/70 on
mutsu (blocked at `use` — see below), so it would inherit that blocker with zero new
information, and it is disqualified anyway (read-only). Its raku run alone took 113
wallclock seconds for 3 tiny files (5 assertions) — the native `LibXML` startup cost is
substantial even under raku.
⁵ Not measured: `XML::XPath` only adds XPath querying on top of `XML`'s own tree (it is
not a distinct reader/writer), and `XML`'s own parser is already the survey's headline
finding (1/15 on mutsu) — measuring the dependent would add no new signal beyond
confirming it inherits the same grammar blocker.
⁶ No `LICENSE`/`LICENCE` file shipped; `README.md` states "licensed under: The Artistic
License 2.0 (GPL Compatible)" in prose. Same weaker-evidence tier as `CSV::Parser` in
[csv.md](csv.md) — not a hard gate, but real weakness on top of the archived-repo signal
below.
⁷ Not measured: ruled out on secondary signals before a mutsu run was worth doing — see
"Ruled out" below.
⁸ Not measured: both are thin extension layers over `LibXML` (a streaming writer, an
XSLT transform binding respectively), not distinct parse+generate candidates in their
own right; each would inherit `LibXML`'s `use`-time blocker regardless.

## What blocks mutsu today

Both real read+generate candidates are **fully healthy under raku** (`XML`: 15/15 files,
149 assertions; `LibXML`: 70/70 files, 723 assertions — the largest, healthiest upstream
suite of any battery survey run so far) and **essentially dead on mutsu**, each behind
its own distinct, now-filed general interpreter bugs (two apiece):

### `XML` — grammar dynamic-variable scoping bug blocks 14 of 15 test files

`XML::Grammar`'s value-parsing token parameterizes itself with a **dynamic variable**
that carries a default (`token value($*STOPPER = '"') { ... }`), then calls a separate
subrule (`token char { ... }`) that reads that dynamic variable back via a code
assertion (`<?{ $*STOPPER eq '"' }>`). This is a standard, documented Raku grammar idiom
for parameterizing a shared subrule from its caller. On mutsu, `$*STOPPER` reads back as
`Nil` inside the subrule — the value set by the caller token's own parameter binding
does not propagate into the dynamic scope of a subrule it calls. Every real
`XML::Grammar.parse` call fails with `could not parse XML` as a result; the only test
file that doesn't touch parsing at all (`t/numeric-entities.rakutest`, pure
string-function tests) passes.

Minimal repro (full detail, including the closer XML-shaped repro, in the ticket):

```raku
grammar G {
    token TOP { <value> }
    token value($*STOPPER = '"') { <char> }
    token char { { say "STOPPER is ", $*STOPPER.raku }; . }
}
G.parse('x');
```

`raku`: `STOPPER is "\""`. `mutsu`: `STOPPER is Nil`.

Ticket: [`todo/tickets/grammar-token-param-dynvar-not-visible-in-subrule.md`](../../todo/tickets/grammar-token-param-dynvar-not-visible-in-subrule.md)

A second, narrower bug blocks the remaining file pair (`t/emitter.rakutest`,
`t/make.rakutest`, both needing `XML::Element.append` → `XML::Node.reparent`):
`XML::Node::reparent`'s parameter uses Raku's indirect/dynamic type-lookup syntax
(`method reparent(::(q<XML::Element>) $parent)`), which mutsu does not accept as a
parameter type constraint. At the top level this is a hard parse failure for the whole
file; inside a `role` body (as here) mutsu instead **silently drops just that one method
declaration** and keeps compiling the rest of the file, which is how the `XML::Node`
role composes into `XML::Element` successfully everywhere else and then fails with
`No such method 'reparent'` only when actually called.

Ticket: [`todo/tickets/indirect-type-param-parse-failure-silently-drops-role-method.md`](../../todo/tickets/indirect-type-param-parse-failure-silently-drops-role-method.md)

### `LibXML` — role meta-invocant + nested-colonpair-alias parameter blocks `use LibXML` entirely

`LibXML::_Configurable` (a role every major `LibXML` class does) declares:

```raku
multi method create(::?ROLE:D :from(:$for)! is raw, |c) {
    self.WHAT.new: :config($for.config), |c
}
```

`::?ROLE:D` is the role's own meta-invocant-type variable; `:from(:$for)!` is a nested
colonpair parameter alias (callable as `:from(...)` or `:for(...)`, bound to `$for`).
Each ingredient works fine alone on mutsu (`::?ROLE:D` with a plain named param; a
nested-colonpair-alias param with a concrete class invocant); only the combination — the
role's own meta-invocant type paired with a nested-colonpair-alias parameter — breaks,
with `Invalid typename 'from' in parameter declaration.` fired at role-body evaluation
time, before any composition or call happens. Since this role is composed early during
`LibXML.rakumod`'s own compilation, `use LibXML;` fails before any user code runs —
mutsu never gets past module load, so all 70 upstream test files fail identically.

Minimal repro:

```raku
role Foo {
    method create(::?ROLE:D :from(:$for)!) { say $for }
}
say "loaded ok";
```

`raku`: `loaded ok`. `mutsu`: `Invalid typename 'from' in parameter declaration.`

Ticket: [`todo/tickets/role-meta-invocant-nested-colonpair-alias-param.md`](../../todo/tickets/role-meta-invocant-nested-colonpair-alias-param.md)

None of these bugs (the two above, plus the two `LibXML` bugs below) are XML-specific —
they are general grammar-dynamic-scoping, parser, and NativeCall gaps that happened to
surface here, in the same spirit as the CSV survey's shared heredoc-in-sub-body bug and
the compression survey's shared NativeCall bugs. The `XML::Grammar` bug in particular
(dynamic variable set by a token's own parameter not visible to a subrule it calls) is a
core regex/grammar-engine correctness gap likely to recur in any hand-rolled Raku grammar
using the same idiom, not just this one.

### `LibXML` — a second, independent blocker even for files that avoid the first

`t/000sanity.t` does not `use LibXML;` (it exercises `LibXML::Raw::Defs` directly), so
it sidesteps the role bug above and gets further — 7 assertions in — before hitting a
second, unrelated NativeCall bug: `LibXML::Raw::Defs` deliberately leaves `$CLIB` as an
**undefined `Str` type object** on Linux (the standard idiom for "resolve this symbol
against the process-global namespace" — `malloc`/`memcpy`/`free` are already linked into
every process). mutsu instead tries to literally `dlopen` a file named `lib(Str).so`:

```
Cannot locate native library 'lib(Str).so': lib(Str).so.2: dlopen failed
```

Minimal repro: `my $CLIB = Str; $CLIB.&cglobal("malloc", Pointer)` throws on mutsu,
returns a valid pointer on raku. Ticket:
[`todo/tickets/nativecall-cglobal-undefined-str-library-mistokenized.md`](../../todo/tickets/nativecall-cglobal-undefined-str-library-mistokenized.md).
This means fixing the role bug alone would not make `LibXML` fully green — this second
bug is an independent blocker for at least this one file, and possibly others using the
same `$CLIB` pattern.

### Building the `LibXML` native shim worked cleanly

For completeness: the `LibXML` `Build.pm6` step (compiling its `xml6` native shim
against system `libxml2`, via `LibraryMake`) was actually run for this survey (not just
assumed) and succeeded with no errors — `gcc`, `make`, and `/usr/include/libxml2` are all
present on this machine, and the resulting `resources/libraries/libxml6.so` loaded and
ran correctly under `raku`. The `use LibXML;` failure above is a pure Raku-level parser
bug in the dist's own `.rakumod` source, unrelated to the native build.

## Ruled out before a full measurement

- **`XML::Writer`** (masak, Artistic-2.0) — **generate-only**: no reader/parser half at
  all, disqualified by this slot's read-and-generate criterion (same rule that dropped
  `CSV::Parser` in [csv.md](csv.md)). Kept in the table for completeness since it is a
  genuinely healthy, small, zero-dependency candidate — **2/2 files (11 assertions)
  under both raku and mutsu**, no interpreter bugs found. Worth revisiting only if the
  slot's criterion is ever relaxed to a generate-only stopgap, which is not recommended
  for the same reason `CSV::Parser` was not recommended as a read-only stopgap in the
  CSV survey: bundling it now would mean re-surveying and likely replacing it once a
  real read+generate candidate clears.
- **`XML::Fast`** (jonathanstowe, Artistic-2.0) — **read-only**: turns XML into a Hash
  structure, no writer/composer API. Disqualified on the same read+generate criterion,
  independent of its `LibXML` dependency chain (which is itself blocked on mutsu — see
  above).
- **`DOM::Tiny`** (HANENKAMP → now maintained by zostay, Artistic-2.0) — ruled out on
  three independent secondary signals, any one of which would deprioritize it: (1) the
  GitHub repository is **archived** (`isArchived: true`), the same near-hard-disqualifier
  selection-method.md calls out; (2) license evidence is README-only, no shipped
  `LICENSE` file (the weaker tier, same as `CSV::Parser`); (3) it is architecturally an
  HTML-first, CSS-selector-driven "relaxed" DOM (a Raku port of Perl 5's `Mojo::DOM58`)
  rather than a strict XML/DOM-standard library — its own test file names
  (`t/023-dom-script-tag.t`, `t/028-dom-barely-html.t`, …) confirm the HTML-tolerance
  focus. Not measured under either raku or mutsu given these three independent strikes
  against it while two much stronger XML-first candidates were already found.
- **`W3C::DOM`** (dwarring, Artistic-2.0) — abstract interface *roles* only ("Abstract
  W3C DOM Level 2 interface roles"), not an implementation; it is `LibXML`'s own
  dependency for DOM-interface conformance, not a standalone candidate.
- **`XML::XPath`** (ufobat, Artistic-2.0) — an XPath query layer built on top of `XML`'s
  own tree, not a distinct reader/writer; inherits `XML`'s blocker regardless. Also the
  stalest repository touched in this survey (last push 2017-07-30).
- **`LibXML::Writer`** and **`LibXSLT`** (both dwarring, Artistic-2.0) — both are thin
  extension layers over `LibXML` (a streaming writer API, an XSLT transform binding),
  not distinct parse+generate candidates; both would inherit `LibXML`'s `use`-time
  blocker regardless of their own code.
- General-purpose/HTML-adjacent tools that merely touch XML as one feature among several
  were excluded from the field entirely per the task's own instruction: `CSS`/
  `CSS::Selector::To::XPath`/`CSS::TagSet` (CSS tooling that tags `xml`/`xpath` as
  related keywords, not XML libraries), `HTML::Parser::XML` (HTML→XML conversion, not a
  general XML parser), `XML::Entity::HTML` (HTML entity encode/decode utility),
  `Audio::Hydrogen`/`Cmark`/`Gnome::Gtk3::Glade`/`Map::Mapnik`/`PDF::Extract`/
  `PDF::Tags::Reader`/`JSON::Path`/`Data::DPath`/`Qwiratry::Format::XML` (XML as one
  input/output format among several in an otherwise unrelated tool).

## Recommendation

Under the stated criteria (read-and-generate, no license gate, native dependency
tolerated per the compression-survey precedent), the field narrows to exactly two live
candidates — `XML` (pure Raku) and `LibXML` (libxml2 binding) — both Artistic-2.0, both
with a clean license cross-check, and both **completely healthy under `raku`** (15/15
and 70/70 files respectively — no candidate in this survey needed to be excluded for
being broken upstream). **Neither is close to usable on mutsu today**, each blocked by
its own general, now-filed interpreter bugs:

1. **`XML` is the stronger candidate on ecosystem standing** — 45 dependents (the
   highest of any battery survey to date, well above `LibXML`'s 7), zero runtime
   dependencies, and matches the task's own expectation that it is the ecosystem's de
   facto standard. It is blocked by a genuine grammar/regex-engine correctness gap
   (dynamic-variable token parameters not visible to a called subrule) that is likely to
   recur elsewhere, making it a good general lever to pull — fixing it probably helps
   more than just this one module. The second, narrower blocker (indirect type-name
   parameter syntax silently dropping a role method) is comparatively easy to fix and
   independently worth doing for its own sake (silently losing a method is a worse
   failure mode than erroring loudly).
2. **`LibXML` is the stronger candidate on feature completeness and native-library
   soundness** — it is the most actively maintained repository found in this entire
   survey (last push 2026-06-10, the same day as this survey's `rea.json` version), has
   the deepest test suite by far (723 assertions vs `XML`'s 149), and its native shim
   built and ran cleanly against the system `libxml2` with zero native-side problems.
   It is blocked purely by a role/parser interaction bug (a nested colonpair-alias
   parameter paired with the role's own `::?ROLE:D` meta-invocant type) that prevents
   `use LibXML;` from completing at all — fixing it is a single, well-isolated parser
   fix, not an architectural gap.
3. **No candidate is bundle-ready today**, and this survey's real output is the four
   filed bugs above rather than a winner — the same "expect the answer to be 'fix mutsu
   first'" outcome selection-method.md documents as normal for this project's current
   stage (see the template and compression surveys for precedent). Given `XML`'s much
   larger ecosystem footprint (45 vs 7 dependents) and its zero-dependency, pure-Raku
   nature (no system-library runtime dependency to carry forward), it is the more
   valuable of the two to prioritize fixing first if only one gets attention — but
   `LibXML`'s blocker looks like the cheaper fix of the two, so a future session might
   reasonably pick it up first purely on lever size.
4. `XML::Writer` (generate-only, 2/2 on mutsu) is a possible generate-only stopgap in
   the same shape `CSV::Parser` was for CSV — **not recommended** for the same reason:
   this slot's own criterion requires both directions, and bundling a half-solution now
   would mean re-surveying and likely replacing it once `XML` or `LibXML` clears.

Re-run this survey (or at least re-measure the four filed bugs) before acting on any of
the above — per selection-method.md, a readiness claim nobody just re-measured is not
evidence.
