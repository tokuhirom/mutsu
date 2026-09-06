# `Config::TOML` battery is blocked on core interpreter campaigns

## Current measurement (2026-09-06)

Both upstream suites re-fetched from GitHub (`raku-community-modules/Crane`,
`raku-community-modules/Config-TOML`) and run from their own directories
against a **debug** build; `Config::TOML` with `Crane/lib` on `MUTSULIB`.

| Suite | raku | mutsu (2026-08-31) | mutsu (2026-09-06, before this round) | mutsu (2026-09-06, after) |
| --- | --- | --- | --- | --- |
| `Config::TOML` v0.1.3 | 19/19 files | 0/19 | **10/19** | **10/19** |
| `Crane` v0.1.2 | 15/15 files | 3/15 | **3/15** | **3/15** |

**The ticket's `Config::TOML 0/19` was badly stale: it is 10/19 today**, and two
of the four listed blockers are gone. `Crane` is still 3/15 at the file level
(`at`, `exists`, `test`) but its assertion-level failures moved a lot this
round — `t/add.rakutest`'s "Original container is unchanged" cluster went 5 → 1.

`Config::TOML` passing: `api/01`, `grammar/01-04`, `grammar-actions/03`,
`special-cases/01`, `04`, `05`, `06`.
`Config::TOML` failing: `dumper/01`, `exceptions/01-02`,
`grammar-actions/01`, `02`, `04`, `special-cases/02`, `03`, `07`.

## Blocker status, re-checked 2026-09-06

1. **Crane's array-path semantics.** Partly stale, partly still open.
   - *Copy isolation* ("Original container is unchanged"): **mostly a
     misdiagnosis.** The failures were not container aliasing at all. Two
     general interpreter bugs produced them and are now fixed:
     - A **sigilless parameter (`\c`) re-read the caller's variable out of
       `env` by name** instead of using the argument the VM evaluated. When the
       live value sat in the caller's local slot and `env` still held the
       declaration-time one — what a cross-compunit method call leaves behind —
       the parameter bound the variable's *type object*, and the exit writeback
       stamped that back onto the caller. Repro needs no module machinery:
       a `unit class C;` with `method m(\c) { 99 }`, called as
       `my Positional $t = <foo bar>; C.m($t)`, left `$t` as `(Positional)`.
       Pin: `t/sigilless-param-reads-argument-not-env.t`.
     - `.isa` answered **False for every role type object** (`class_mro` on a
       role name yields just that name), so Crane's `ok($t.isa(Any))` on an
       undefined `Associative`/`Positional`-typed scalar failed. raku answers
       from the role's pun's chain — exactly `Any` and `Mu`. A class whose only
       declared parents are composed roles (`class K does A`) had the twin bug.
       Pin: `t/isa-role-type-object-and-role-only-parent.t`.
   - *`Crane::List`* (and every other `unit class Crane::X;` file): the `::`
     segments of a **compound declared name** were treated as enclosing lexical
     scopes, so `List.new(...)` inside `class Crane::List` constructed a
     `Crane::List` ("Default constructor for 'Crane::List' only takes named
     arguments") instead of a core `List`. Fixed by recording, at declaration
     time, whether a registry key's segments came from a compound name or from
     real nesting (`unit module NL; class Searcher`) and skipping the former in
     the type-name walk. Pin: `t/compound-declared-name-is-not-a-scope.t`.
     NOTE: bare *routine* lookup deliberately still crosses those segments —
     mutsu also uses them to model a module compunit's file-scope lexicals
     (`HTTP::HPACK`'s own `sub decode-int`, reached from
     `HTTP::HPACK::Decoder`'s methods), and cutting that walk breaks bundled
     modules. So `class Quux::User { method m { greet() } }` still prefers
     `Quux::greet` over the file's own `greet`, where raku picks the file's.
     That residual divergence is unfixed.
   - *Positional-index classification* (`X::Crane::PositionalIndexInvalid`
     raised by `Crane::Utils`' enum-value multis): **stale — this works.** The
     whole `is-valid-positional-index` / `gen-classifier` chain, including
     `WhateverCode` (`*-0`) steps, matches raku exactly on a standalone repro.
     What still fails in `t/in.rakutest` is the *descent* around it, not the
     classifier.
   - *Still open:* the out-of-range / sparse-Positional exception family. Crane
     wraps `splice` in a `CATCH { when X::OutOfRange { ... } }` and re-throws
     `X::Crane::AddPathOutOfRange`; mutsu's `splice` does raise `X::OutOfRange`
     with the right message on its own, so the gap is somewhere in the descent
     or the `CATCH` mapping. Not bisected. This is now the dominant Crane
     cluster: `add` 6× "code dies" + 5× wrong exception type, and the same
     shape in `copy`, `move`, `remove`, `replace`, `set`, `patch`.
2. ~~`t/patch.rakutest` fails to parse.~~ Fixed 2026-08-31.
3. ~~The 8-hex `\UXXXXXXXX` string escape.~~ **Fixed** — `grammar/04` and
   `grammar-actions/04`'s grammar half both pass now; `grammar-actions/04`
   still fails for an unrelated reason.
4. ~~`t/grammar/03-inline-tables.rakutest` times out.~~ **Fixed** — it passes,
   and so does `grammar-actions/03`.

New this round, filed separately:

5. `todo/tickets/object-hash-key-lost-when-pair-value-is-a-container.md` — the
   whole remaining blocker for `Crane`'s `flatten` and `list` files.
   `my Any:D %t{List:D} = (%h<path> => %h<value>,)` dies with a bogus
   `expected List:D but got Str ("1 2")` because the Pair's *value* is a
   write-through `ContainerRef` and the object hash then loses the key object.
   Also records two smaller measured divergences (`=>` does not decontainerize
   its key; an itemized list used as a hash subscript is flattened into a
   slice).

## What this ticket is

`Config::TOML` v0.1.3 + its dependency `Crane` v0.1.2 (both
`auth<zef:raku-community-modules>`, Unlicense) won the TOML-parser
battery-slot survey — see `docs/batteries/toml.md` for the full field, the
metrics, and why it beat `TOML` (zef:tony-o) and `TOML::Thumb`. It is
recorded in `BATTERIES.md` §7 as **Selected, not yet bundled**.

This ticket is the **follow-up mechanical step** — vendoring + wiring it up as
an actual battery — once its blockers clear. **Do not start the vendoring steps
yet**: `Crane` at 3/15 is still too thin for a per-file whitelist to be worth
it, and `Config::TOML` builds every result through `Crane.set`/`Crane.exists`.

## Steps (once unblocked)

Follow the standard vendoring recipe,
[BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), using
`docs/batteries/templates.md` (`Template::Mustache`) as the shape of a
finished record and PR:

1. **Re-run the survey** to get current pass counts: fetch both dists fresh
   (`docs/batteries/toml.md`'s provenance table has the exact upstream URLs
   and pinned versions) and run their suites under `raku` and a release
   `target/release/mutsu` build.
2. **Vendor** `lib/` + `META6.json` + `UNLICENSE` + `README.md` for both
   modules into `modules/Config-TOML/` and `modules/Crane/` (new
   directories, following the `modules/<Dist-Name>/` naming already used by
   every other entry). Exclude upstream `t/`, `run-tests`, `dist.ini`,
   `doc/`.
3. **Register the default module search path** entry the same way every
   other bundled module is wired (check `add_default_site_repo()` / the
   `modules/` tree registration mentioned in
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution)).
4. **`batteries.lock`**: add entries for `Config::TOML` (commit pinned to
   v0.1.3) and `Crane` (commit pinned to v0.1.2), then run
   `scripts/battery-testsuite.sh --update` and review the
   `batteries-whitelist.txt` diff — a test file that doesn't make the
   whitelist is a gap to note in the record, not silently drop.
5. **Smoke test**: `mutsu -e 'use Config::TOML; say from-toml(q[[a]]{Xb}=1)'`
   (or equivalent) round-trips.
6. **Update `docs/batteries/toml.md`**: flip the status line from "Selected,
   not yet bundled" to "Working" (or a partial-pass note if the whitelist
   isn't 19/19), fill in the actual commit hashes in the provenance table,
   add the vendor-recipe `rsync` commands.
7. **Update `BATTERIES.md` §7**'s TOML row: change `Kind` from
   `**Selected, not yet bundled**` to `Adopted`, and rewrite the `Status`
   cell to match whatever `docs/batteries/toml.md` now says.
8. **`site/batteries.html`**: add the TOML row per
   [BATTERIES.md §5](../../BATTERIES.md#publish-the-bundle-on-the-pages-site)
   — only once it is actually "Working", not before.
9. Regenerate the manifest if one exists for the bundle
   (`python3 scripts/gen-batteries-manifest.py`).

## How to re-measure

```
mkdir -p tmp/toml-survey && cd tmp/toml-survey
git clone --depth 50 https://github.com/raku-community-modules/Crane.git
git clone --depth 50 https://github.com/raku-community-modules/Config-TOML.git
# Crane:        (cd Crane && mutsu -I lib t/<file>.rakutest)
# Config::TOML: (cd Config-TOML && MUTSULIB=../Crane/lib mutsu -I lib t/<dir>/<file>.rakutest)
# raku baseline: same, with `raku -Ilib [-I../Crane/lib]`
```

Run every failing file under `raku` on the same checkout before calling
anything a mutsu bug — three of this ticket's four listed blockers turned out
to be already fixed or misdiagnosed when that was actually done.
