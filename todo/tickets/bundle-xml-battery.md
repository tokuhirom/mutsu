# Vendor and bundle `XML` once its remaining core blocker is fixed

## What this is

`XML` v0.3.6 (`auth<zef:raku-community-modules>`, Artistic-2.0) won the XML
parse+generate battery-slot survey — see
[docs/batteries/xml.md](../../docs/batteries/xml.md) for the full field, the
metrics table, and why it beat `LibXML` (a `libxml2` NativeCall binding: 7
dependents vs. `XML`'s 45, plus a hard system-library dependency `XML`
doesn't carry). It is recorded in `BATTERIES.md` §7 as **Selected, not yet
bundled**.

This ticket is the **follow-up mechanical step** — vendoring + wiring it up
as an actual battery — once its blockers clear. It is intentionally separate
from the blockers themselves so no interpreter fix is scoped to also cover
packaging/docs/CI work.

## Current measurement (2026-08-26)

Re-measured from a fresh REA fetch of v0.3.6, running the upstream suite from
the dist's own directory against a release build. `raku`: **15/15** files.

| Point in time | mutsu |
| --- | --- |
| Original survey (2026-08-22) | 1/15 |
| After the two originally-filed blockers were fixed | 2/15 |
| After the group-backreference fix (2026-08-26) | 5/15 |
| After the `$self`/invocant fix, ADR-0061 (2026-08-27) | **9/15** |

Passing: `comments`, `custom-entities`, `emitter`, `entities`,
`numeric-entities`, `parser`, `preamble`, `query-positional`, `quotes`.

### Blockers cleared since the survey

1. ~~A token's own dynamic-variable parameter default is not visible inside a
   subrule it calls.~~ **Fixed** —
   [news/2026-08/grammar-token-param-dynvar-not-visible-in-subrule.md](../../news/2026-08/grammar-token-param-dynvar-not-visible-in-subrule.md).
   Re-verified 2026-08-26 with the record's own minimal repro.
2. ~~An indirect type-name parameter constraint silently drops a role method.~~
   **Fixed** — `XML::Node::reparent` composes, and `t/emitter.rakutest` passes.
3. ~~A backreference inside a `[...]` group does not resolve against the
   enclosing pattern's captures.~~ **Found and fixed during this
   re-measurement** —
   [news/2026-08/regex-backref-inside-a-group.md](../../news/2026-08/regex-backref-inside-a-group.md).
   This was hidden behind blocker 1: `XML::Grammar`'s `element` token closes
   with `[ '/>' | '>' <child>* '</' $<name> '>' ]`, so no element with a
   closing tag matched at all (`<root/>` parsed, `<root></root>` did not).

4. ~~A user lexical `$self` collides with a method's invocant, so
   `XML::Element`'s `my $self = self;` + `Proxy` `AT-POS`/`AT-KEY` recursed
   into `FETCH` until the stack overflowed.~~ **Fixed 2026-08-27** —
   [ADR-0061](../../docs/adr/0061-lexical-self-has-its-own-env-key.md) /
   [news/2026-08/lexical-self-has-its-own-env-key.md](../../news/2026-08/lexical-self-has-its-own-env-key.md).
   This took the suite from **5/15 to 9/15**: every file that aborted on
   `$doc.root[0]` now runs.

### What still blocks it

Re-measured 2026-08-27 (debug build, `mutsu -I lib t/*.rakutest` from the
dist's own directory): **9/15**, `raku` 15/15. The six remaining failures:

1. **`XML::Document` does not delegate postcircumfix to its root element** —
   `t/proxies.rakutest`, `t/query-methods.rakutest`, `t/example.rakutest`.
   `$doc[1]` / `$doc<attr>` return `(Any)` where the element-level
   `$doc.root[1]` now works, and `.attribs` on the document is `(Any)` too.
2. **A missing `.string` method on `XML::Element`** (`t/proxies.rakutest`
   aborts on it after the assertions above).
3. **Three files still not bisected**: `t/make.rakutest` (`make-xml worked.`
   fails), `t/namespaces.rakutest` (2 assertions: default-namespace content,
   `elements(:URI)`), `t/open-xml.rakutest` (exits 255).

Two neighbouring `Proxy` defects found while fixing blocker 4 are filed
separately and may well be behind item 1:
[proxy-at-pos-store-and-shadowed-capture.md](proxy-at-pos-store-and-shadowed-capture.md)
and [proxy-what-reports-proxy-instead-of-fetching.md](proxy-what-reports-proxy-instead-of-fetching.md).

**Re-measure before starting** — per `selection-method.md`, a readiness claim
nobody just re-measured is not evidence.

## Follow-up re-measurement (2026-08-31): still blocked at 9/15

The required fresh measurement was repeated from the v0.3.6 tag
(`0349d282e257be61075f55abfde4c42a01bc8f10`) with a release mutsu build, from
the distribution's own checkout and with `-I lib`. mutsu remains at **9/15**
files (141 assertions reached); `raku -I lib` passes **15/15** (149
assertions). The same six files fail: `example`, `make`, `namespaces`,
`open-xml`, `proxies`, and `query-methods`.

The apparent `XML::Document` postcircumfix defect was traced one layer deeper.
Outside a method, `$doc.root[1]` reads correctly. From inside an
`XML::Document` method, however, an `XML::Element` `AT-POS` Proxy's `FETCH`
closure resolves its captured `my $self = self` through the calling frame's
same-named environment rather than the lexical binding created by
`XML::Element::AT-POS`. This makes the callback see a `Str`/`Any` instead of
the element, so `$doc[1]`, nested XML walks, and the downstream `.string`
calls fail.

This is not safe to repair as an XML-specific Proxy exception. It is an open
instance of [ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md):
closure captures must resolve to their creating binding, while mutable captures
need shared cells for freshness. ADR-0055 records that the broad
caller-priority-to-closure-priority merge change still needs its cell-coverage
prerequisite; an earlier prototype regressed sequential `Cro::HTTP` requests.
The remaining XML suite failures therefore stay blocked on general interpreter
work, and this mechanical bundling ticket must remain open.

## Steps (once unblocked)

Follow the standard vendoring recipe,
[BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), using
`docs/batteries/logging.md` (`Log::Async`, bundled 2026-08-26) as the shape of
a finished record and PR — it is the most recent worked example, including a
partial-whitelist battery and a re-vendoring recipe.

1. **Re-run the survey** to get a current pass count (fetch fresh from the REA
   `source-url` in `docs/batteries/xml.md`, run its `t/*.rakutest` under a
   release build from the dist's own directory with `-I lib`).
2. **Vendor** `lib/` + `META6.json` + `LICENSE` + `README.md` into
   `modules/XML/`. Exclude upstream `t/`, `xt/`, `.github/`, and any
   `.precomp` artifacts (a `raku` run inside the checkout leaves those
   behind — check before copying).
3. **No wiring code is needed**: `resolve_bundled_lib_paths()` registers every
   `modules/<Dist>/lib` that exists, so creating the directory is the whole
   registration step.
4. **`batteries.lock`**: add a row for `XML` pinned to the v0.3.6 commit, then
   run `scripts/battery-testsuite.sh --update` and review the
   `batteries-whitelist.txt` diff — a test file that doesn't make the
   whitelist is a gap to note in the record, not silently drop.
5. **Smoke test**: `t/xml-battery.t` — parse a small XML string into a DOM
   tree, walk it, and round-trip it back to a string via `.Str`/`.emit`
   (round-tripping exercises both halves the slot's selection criterion
   requires).
6. **Update `docs/batteries/xml.md`**: flip `**Kind:**` from `Selected, not yet
   bundled` to `Adopted`, rewrite the Status section to match the re-run, and
   fill in a provenance section with the vendored commit plus the exact
   re-vendoring recipe (BATTERIES.md §3 requires the recipe).
7. **Update `BATTERIES.md` §7**'s XML row to Adopted with the real numbers.
8. **`site/batteries.html`**: add the sidecar entry to
   `scripts/gen-batteries-manifest.py` and re-run
   `python3 scripts/gen-batteries-manifest.py` — the page is generated from
   `modules/*/META6.json`, not hand-edited.

## Why this is a `tickets/` item, not `deep/`

No design decision is needed for the *bundling*: the candidate, its license,
and the vendoring recipe are all decided and documented. The remaining
interpreter blocker is `deep/`, and lives there.
