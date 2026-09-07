# `Config::TOML` battery is blocked on core interpreter campaigns

## Current measurement (2026-09-07)

Both upstream suites re-fetched from GitHub (`raku-community-modules/Crane`,
`raku-community-modules/Config-TOML`) and run from their own directories
against a **debug** build; `Config::TOML` with `Crane/lib` on `MUTSULIB`.

| Suite | raku | mutsu (2026-08-31) | mutsu (2026-09-06) | mutsu (2026-09-07, before) | mutsu (2026-09-07, after) |
| --- | --- | --- | --- | --- | --- |
| `Config::TOML` v0.1.3 | 19/19 files | 0/19 | 10/19 | 10/19 | **11/19** |
| `Crane` v0.1.2 | 15/15 files | 3/15 | 3/15 | 3/15 | **4/15** |

The 2026-09-06 numbers reproduced exactly, so that pass's re-measurement was
sound. This pass fixed four general interpreter bugs (below) and moved both
counts: `Config::TOML`'s `exceptions/01-parser.rakutest` went from 12 failing
assertions to a clean 17/17, and `Crane`'s `get.rakutest` now passes.

`Config::TOML` passing: `api/01`, `exceptions/01`, `grammar/01-04`,
`grammar-actions/03`, `special-cases/01`, `04`, `05`, `06`.
`Config::TOML` failing: `dumper/01`, `exceptions/02`, `grammar-actions/01`,
`02`, `04`, `special-cases/02`, `03`, `07`.
`Crane` passing: `at`, `exists`, `get`, `test`.

## Fixed 2026-09-07 (general interpreter bugs, all pinned)

1. **A container element's `++`/`--` seeded from `Int` 0 regardless of the
   declared element type.** `my Bool:D %h; %h<a>++` died with
   `Type check failed for an element of %h; expected Bool:D but got Int (1)`;
   raku answers `True` (`postfix:<++>` is `.=succ`, and an uninitialized
   `Bool` slot succs to `True`). The scalar path already seeded from the
   declared type (`normalize_incdec_source_with_type`); the element path did
   not. Now both share `incdec_seed_for_constraint`, which also strips a
   `:D`/`:U`/`:_` smiley before the lookup.
2. **An object hash's element `++`/`--` keyed by the display string.**
   `my %h{Int}; %h{5}++; say %h{5}` read back the type object, because the
   store used `idx.Str` while every read (and every `=` store) uses `.WHICH`;
   and `.keys` handed back a `Str` because nothing recorded the key object in
   `original_keys`. `exec_inc_dec_index_op` now detects the object hash the
   same way the assign path does and takes the `.WHICH`/`original_keys` route.
3. **An exception thrown by a `where` constraint was swallowed and read as
   "this candidate does not match".** Raku propagates it out of the whole
   dispatch. This is the mechanism the *whole* of `Crane` is built on -- its
   `at`/`in`/`add`/`set` descent is a chain of
   `multi sub at(Positional:D $c, @steps where { ... is-valid-positional-index(@steps[0]) ... })`
   candidates whose `where` *dies* to classify a bad index. mutsu fell through
   to the next candidate and reported the wrong exception type everywhere.
   The matchers are `bool`-returning predicates, so the exception is stashed in
   `Interpreter::pending_where_exception` and re-raised by the dispatch funnel
   before any candidate body runs, with `exec_one` as the backstop that
   guarantees it can never be dropped. A control-flow signal
   (`return`/`next`/...) is not an exception and still reads as "no match".
   The exception escapes only when raku would have *reached* that candidate:
   mutsu evaluates every candidate's matcher to rank them, while rakudo walks
   them narrowest-first and stops at the first that binds, so
   `choose_best_matching_candidate` discards the stash when a nominally
   narrower candidate matched. (Comparing NOMINAL narrowness -- mutsu's own
   ranking weighs a `where` above the parameter shape, which is right for
   picking a winner but would wrongly claim the thrower came first;
   `roast/S06-multi/proto.t`'s `multi bar(| (A $x))` vs
   `multi bar(| where { $_[0] == 42 })` pins exactly that.)
4. **An unsupplied parameter with a DEFAULT skipped its `where` entirely**, so
   every such candidate matched and the first one declared always won.
   Raku evaluates the default and applies the `where` to it, which is how
   `X::Crane::PositionalIndexInvalid`'s
   `multi method message(Str:D $c where { $_ eq 'INTM' } = $.classifier)` pair
   picks its message. Fixing it also required binding `self` during method
   candidate matching -- a default (or a `where`) that reads `$.attr` had no
   invocant in scope.

Pins: `t/incdec-element-typed-seed-and-object-hash-key.t`,
`t/where-clause-exception-and-defaulted-param-dispatch.t`.

## Blocker status, re-checked 2026-09-07

1. **Crane's array-path semantics.**
   - *Positional-index classification* (`X::Crane::PositionalIndexInvalid`):
     **closed** by fixes 3 + 4 above. `Crane.get(%data, :path<legumes foo>)`
     now raises the right type *and* the right message.
   - *Copy isolation* ("Original container is unchanged"): still 1 failure in
     `add.rakutest` and the dominant cluster in `transform.rakutest`. The
     2026-09-06 diagnosis (two general bugs, since fixed) accounted for most of
     it; what is left is a genuine `is rw` / `return-rw` descent that mutates
     the caller's container when Crane asked for a copy.
   - *The `X::OutOfRange` / `CATCH` re-throw descent* was **not** a single
     cluster and was **not** what the 2026-09-06 note guessed. Bisected this
     pass: `splice` itself is correct (`@a.splice(*-2, 0, 'x')` on an empty
     array raises `X::OutOfRange` with raku's exact message). Two separate
     residues remain: (a) `$list.splice(...)` on an immutable `List` raises
     `X::Immutable` where raku raises `X::Multi::NoMatch`, which Crane's
     `CATCH` maps to `X::Crane::Add::RO` only in raku's spelling; (b) a
     `splice` reached through Crane's `*-0` path inserts at index 0 instead of
     at the end (`add.rakutest`'s two "Is expected value" failures show the
     spliced `0..11` landing first, not last).
   - *`Crane::List` / `Crane::Flatten`*: still the object-hash ticket, see 5.
2. ~~`t/patch.rakutest` fails to parse.~~ Fixed 2026-08-31.
3. ~~The 8-hex `\UXXXXXXXX` string escape.~~ Fixed 2026-09-06.
4. ~~`t/grammar/03-inline-tables.rakutest` times out.~~ Fixed 2026-09-06.
5. `todo/tickets/object-hash-key-lost-when-pair-value-is-a-container.md` --
   still the whole remaining blocker for `Crane`'s `flatten` and `list` files.
6. **New this pass, unfixed:** `.WHICH` of an `Array` is mutsu's Gc pointer in
   some paths and the content string (`Array|a b`) in others, so
   `my %h{Array:D}; %h{$k} = 1` and a later `%h{$k}` read agree with each other
   but neither agrees with a pointer-keyed write. Fix 2 above deliberately
   keys `++` the way the read and `=` paths already key, so nothing regressed,
   but an object hash keyed by a *mutable* container is still not identity-keyed
   the way Rakudo keys it. Not worth a campaign for these two dists (neither
   looks an `Array` key up by hash; `Config::TOML` compares with `eqv`).
7. **New this pass, unfixed (`Config::TOML` residue):** `grammar-actions/01`
   is 3 byte-for-byte string-equivalence failures plus an `Int` coercion
   reporting `''` instead of `(Int)`; `grammar-actions/02` is one Rat/FatRat
   precision digit (`9224617.445991228313` vs `9224617.445991227`);
   `dumper/01` + `exceptions/02` are the TOML *dumper*, untouched this pass.

## What this ticket is

`Config::TOML` v0.1.3 + its dependency `Crane` v0.1.2 (both
`auth<zef:raku-community-modules>`, Unlicense) won the TOML-parser
battery-slot survey — see `docs/batteries/toml.md` for the full field, the
metrics, and why it beat `TOML` (zef:tony-o) and `TOML::Thumb`. It is
recorded in `BATTERIES.md` §7 as **Selected, not yet bundled**.

This ticket is the **follow-up mechanical step** — vendoring + wiring it up as
an actual battery — once its blockers clear. **Do not start the vendoring steps
yet**: `Crane` at 4/15 is still too thin for a per-file whitelist to be worth
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
anything a mutsu bug — three of this ticket's four originally listed blockers
turned out to be already fixed or misdiagnosed when that was actually done, and
the 2026-09-07 pass found the "not bisected" `X::OutOfRange` cluster was two
unrelated residues rather than one.
