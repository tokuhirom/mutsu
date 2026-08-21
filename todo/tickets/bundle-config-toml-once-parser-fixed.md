# Vendor and bundle `Config::TOML` once its remaining core blockers are fixed

## What this is

`Config::TOML` v0.1.3 + its dependency `Crane` v0.1.2 (both
`auth<zef:raku-community-modules>`, Unlicense) won the TOML-parser
battery-slot survey — see `docs/batteries/toml.md` for the full field, the
metrics, and why it beat `TOML` (zef:tony-o) and `TOML::Thumb`. It is
recorded in `BATTERIES.md` §7 as **Selected, not yet bundled**: the decision
is made, but the module does not run on mutsu yet, so there is nothing to
vendor or gate today.

This ticket is the **follow-up mechanical step** — vendoring + wiring it up
as an actual battery — once its blockers clear. It is intentionally separate
from the blockers themselves so none of those interpreter fixes is scoped to
also cover packaging/docs/CI work.

## Blocked on

The original blocker (bare-identifier adverb declaration names) landed on
2026-08-22, so `Config::TOML` now loads and its grammar parses correctly. It is
still 0/19 at the file level; `docs/batteries/toml.md`'s **remaining work
list** carries the current blockers, largest first:

1. `todo/deep/is-rw-lvalue-return-is-caller-side-ast-reinterpretation.md` —
   `Crane.set` silently does nothing, so `from-toml` returns `{}`. Nothing in
   `Config::TOML` can produce a result until this lands.
2. `todo/tickets/ordered-alternation-loser-branch-code-block-fires.md` — any
   TOML document containing a `\n`/`\"`/`\\` escape dies.
3. `todo/tickets/push-with-slip-arg-in-sink-context.md`.
4. Assorted un-bisected per-assertion failures (see the record).

**Do not start this ticket before those are merged and `Config::TOML` +
`Crane`'s suites have been re-run to confirm they now pass** (or at least pass
enough files to be worth a per-file whitelist, the same way
`CBOR::Simple`/`Log::Timeline` ship as "Sufficient for Cro" with partial
coverage).

## Steps (once unblocked)

Follow the standard vendoring recipe,
[BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), using
`docs/batteries/templates.md` (`Template::Mustache`) as the shape of a
finished record and PR:

1. **Re-run the survey** to get current pass counts: fetch both dists fresh
   (`docs/batteries/toml.md`'s provenance table has the exact upstream URLs
   and pinned versions/commits) and run their suites under `raku` and a
   release `target/release/mutsu` build. Confirm which of the 19
   `Config::TOML` files (plus `Crane`'s own ~15 `.rakutest` files under
   `t/`) actually pass now — the parser fix may not clear 100% on the first
   try.
2. **Vendor** `lib/` + `META6.json` + `UNLICENSE` + `README.md` for both
   modules into `modules/Config-TOML/` and `modules/Crane/` (new
   directories, following the `modules/<Dist-Name>/` naming already used by
   every other entry). Exclude upstream `t/`, `run-tests`, `dist.ini`,
   `doc/`.
3. **Register the default module search path** entry the same way every
   other bundled module is wired (check `add_default_site_repo()` / the
   `modules/` tree registration mentioned in
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution) — no new
   mechanism needed, this is just adding two more directories to an
   existing list/manifest).
4. **`batteries.lock`**: add entries for `Config::TOML` (commit pinned to
   v0.1.3) and `Crane` (commit pinned to v0.1.2), then run
   `scripts/battery-testsuite.sh --update` and review the
   `batteries-whitelist.txt` diff — a test file that doesn't make the
   whitelist is a gap to note in the record, not silently drop.
5. **Smoke test**: `mutsu -e 'use Config::TOML; say from-toml(q[[a]]{Xb}=1)'`
   (or equivalent) round-trips.
6. **Update `docs/batteries/toml.md`**: flip the status line from "Selected,
   not yet bundled" to "Working" (or "Sufficient for X" / a partial-pass
   note if the whitelist isn't 19/19), fill in the actual commit hashes in
   the provenance table, add the vendor-recipe `rsync` commands.
7. **Update `BATTERIES.md` §7**'s TOML row: change `Kind` from
   `**Selected, not yet bundled**` to `Adopted`, and rewrite the `Status`
   cell to match whatever `docs/batteries/toml.md` now says.
8. **`site/batteries.html`**: add the TOML row per
   [BATTERIES.md §5](../../BATTERIES.md#publish-the-bundle-on-the-pages-site)
   — only once it is actually "Working", not before.
9. Regenerate the manifest if one exists for the bundle
   (`python3 scripts/gen-batteries-manifest.py`, same as the
   `Template::Mustache` recipe).

## Why this is a `tickets/` item, not `deep/`

No design decision is needed — the candidate, its license, and the vendoring
recipe are all already fully decided and documented in `docs/batteries/toml.md`.
This is pure mechanical follow-through once its blocking bugs are fixed.
