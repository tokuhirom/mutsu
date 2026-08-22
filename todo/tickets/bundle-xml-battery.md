# Vendor and bundle `XML` once its two remaining core blockers are fixed

## What this is

`XML` v0.3.6 (`auth<zef:raku-community-modules>`, Artistic-2.0) won the XML
parse+generate battery-slot survey — see
[docs/batteries/xml.md](../../docs/batteries/xml.md) for the full field, the
metrics table, and why it beat `LibXML` (a `libxml2` NativeCall binding: 7
dependents vs. `XML`'s 45, plus a hard system-library dependency `XML`
doesn't carry). It is recorded in `BATTERIES.md` §7 as **Selected, not yet
bundled**: the decision is made, but the module does not run on mutsu yet,
so there is nothing to vendor or gate today.

This ticket is the **follow-up mechanical step** — vendoring + wiring it up
as an actual battery — once its blockers clear. It is intentionally separate
from the blockers themselves so neither interpreter fix is scoped to also
cover packaging/docs/CI work (same split already used for `Config::TOML`'s
[bundle-config-toml-once-parser-fixed.md](bundle-config-toml-once-parser-fixed.md)
and `Log::Async`'s
[bundle-log-async-battery.md](bundle-log-async-battery.md)).

## Blocked on

Both of these must land and be re-verified before starting — `docs/batteries/xml.md`'s
["What blocks mutsu today"](../../docs/batteries/xml.md#what-blocks-mutsu-today)
section has the full repros:

1. [todo/tickets/grammar-token-param-dynvar-not-visible-in-subrule.md](grammar-token-param-dynvar-not-visible-in-subrule.md) —
   `XML::Grammar`'s value-parsing token sets a dynamic variable via its own
   parameter default (`token value($*STOPPER = '"') {...}`) and a subrule it
   calls reads that variable back as `Nil` instead of the value the caller
   set. This breaks nearly every real `XML::Grammar.parse` call — 13 of
   `XML`'s 15 upstream test files fail this way.
2. [todo/tickets/indirect-type-param-parse-failure-silently-drops-role-method.md](indirect-type-param-parse-failure-silently-drops-role-method.md) —
   `XML::Node::reparent`'s indirect type-name parameter syntax
   (`method reparent(::(q<XML::Element>) $parent)`) is not accepted as a
   parameter type constraint; inside a role body mutsu silently drops just
   that one method instead of erroring, so `XML::Element.append` fails at
   call time with `No such method 'reparent'`. Blocks the remaining 2 files
   (`t/emitter.rakutest`, `t/make.rakutest`).

**Do not start this ticket until both are merged and `XML`'s own upstream
suite has been re-run to confirm the pass count actually improved** — per
`selection-method.md`, a readiness claim nobody just re-measured is not
evidence. A partial improvement (e.g. bug 1 fixed but bug 2 still open) is
fine to act on if the remaining 2 files are worth gating separately, same as
`CBOR::Simple`/`Log::Timeline` ship as "Sufficient for Cro" with partial
coverage — but re-measure first either way.

## Steps (once unblocked)

Follow the standard vendoring recipe,
[BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), using
`docs/batteries/csv.md` (`Text::CSV`) as the shape of a finished record and
PR:

1. **Re-run the survey** to get a current pass count: fetch `XML` fresh (the
   REA `source-url` is in `docs/batteries/xml.md`'s field table) and run its
   upstream `t/*.rakutest` suite under a release `target/release/mutsu`
   build from the dist's own directory (`-I lib`). Confirm how many of the
   15 files pass now.
2. **Vendor** `lib/` + `META6.json` + `LICENSE` + `README.md` into
   `modules/XML/` (new directory, following the `modules/<Dist-Name>/`
   naming already used by every other bundled battery). Exclude upstream
   `t/`, `xt/`, `.github/`, precomp artifacts.
3. **Register the default module search path** entry the same way every
   other bundled module is wired — no new mechanism needed, this is adding
   one more directory to the existing `modules/` tree registration
   ([BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution)).
4. **`batteries.lock`**: add an entry for `XML` pinned to the v0.3.6 commit,
   then run `scripts/battery-testsuite.sh --update` and review the
   `batteries-whitelist.txt` diff — a test file that doesn't make the
   whitelist is a gap to note in the record, not silently drop.
5. **Smoke test**: `t/xml-battery.t` — parse a small XML string into a DOM
   tree, walk it, and round-trip it back to a string via `.Str`/`.emit`
   (round-tripping exercises both the parse and generate halves the slot's
   selection criterion requires).
6. **Update `docs/batteries/xml.md`**: flip the header's `**Kind:**` from
   `Selected, not yet bundled` to `Adopted`, rewrite the
   ["Status" section](../../docs/batteries/xml.md#status-selected-not-yet-bundled)
   to match whatever the re-run actually shows, and fill in the real
   vendored commit hash in a provenance section (add one if the record
   doesn't have one yet — follow `docs/batteries/uuid.md`'s shape).
7. **Update `BATTERIES.md` §7**'s XML row: change `Kind` from `**Selected,
   not yet bundled**` to `Adopted`, and rewrite the summary cell to match.
8. **`site/batteries.html`**: add the XML row per
   [BATTERIES.md §5](../../BATTERIES.md#5-documentation-requirement) — only
   once it is actually "Working", not before.
9. Regenerate the manifest if one exists for the bundle
   (`python3 scripts/gen-batteries-manifest.py`, same as the
   `Template::Mustache` recipe).

## Why this is a `tickets/` item, not `deep/`

No design decision is needed — the candidate, its license, and the
vendoring recipe are all already fully decided and documented in
`docs/batteries/xml.md`. This is pure mechanical follow-through once its
two blocking bugs are fixed.
