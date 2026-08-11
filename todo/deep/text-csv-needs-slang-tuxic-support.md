# `Text::CSV` needs `use Slang::Tuxic;` to even parse — no slang-switching architecture exists

## Discovered while

Surveying CSV libraries on the ecosystem (`docs/batteries/csv.md`). `Text::CSV`
is the deepest-tested, most CPAN-familiar candidate for the CSV battery slot
(33/33 files, 22697 assertions under raku) but cannot be `use`d on mutsu at
all — not a runtime bug, a parse-time one, and unlike the heredoc-scope false
positive (`news/2026-08/heredoc-scope-check-false-positive-on-sub-body.md`,
fixed) this one is not a mutsu bug to fix — it is a missing architectural
capability.

## Repro

```
$ mutsu -I lib -e 'use Text::CSV; say "ok"'
===SORRY!=== Error while compiling -e
expected statement: expected expected statement: expected right-hand
expression after '=' or Confused. no space allowed between method name and
the left parenthesis or expression statement
```

`Text::CSV.rakumod:3` has `use Slang::Tuxic;` at the top of the file (also in
`lib/Text/IO/String.rakumod:1` and `lib/V.rakumod:4`), and the failure is a
parse error in `Text::CSV.rakumod` itself — before any user code, before any
of the heredoc content that was this survey's other finding.

## What `Slang::Tuxic` actually does

Not a normal library — it is a genuine Raku **slang**: a `role` that gets
mixed into the live Raku grammar at compile time, overriding grammar rules
(`~/.zef/store/Slang-Tuxic-*/lib/Slang/Tuxic.rakumod`):

```raku
my role Tuxic {
    token routine-declarator:sym<sub> { ... }
    token term:sym<identifier> { ...; <?before <.unspace>|\s*'('> \s* <![:]> ...; <args> }
    token methodop(Mu $*DOTTY) { ... }
}
```

Its purpose: allow whitespace between a sub/method name and its opening `(`
(`func (args)` treated the same as `func(args)`, and `.method (args)` the
same as `.method(args)`) — author H.Merijn Brand's personal style preference,
declared in `Text::CSV`'s own README as "to support my style". It is not
load-bearing for CSV parsing logic itself, but it is used pervasively
throughout `Text::CSV.rakumod` (~500 call sites match the spaced form,
though many of those hits are inside string literals for error messages
rather than real syntax) and is `use`d unconditionally at the top of the
file, so there is no way to `use Text::CSV;` without the slang taking
effect.

## Why this is architectural, not a parser bug

Raku's grammar is not monolithic — it switches between sub-languages
("slangs": Main, Regex, Quote, Pod, and user-defined ones via role mixins
like this) depending on context. `CLAUDE.md`'s own "Raku's context-dependent
parsing (slangs)" section states this plainly: **"The parser does not
natively support slang switching"** — mutsu's parser is a hand-written Rust
recursive-descent parser with no notion of a pluggable/mixin-based grammar at
all, whereas `Slang::Tuxic` relies on Rakudo's `RakuAST`/NQP-level mechanism
for dynamically patching grammar rules via role composition. There is no
"add one flag" fix here; supporting `use Slang::Tuxic;` faithfully requires
mutsu to have *some* notion of a parser that can be told, mid-file, "some
token/term rules are different from here on" — which does not exist.

## DECIDED (2026-08-11): option 1, per ADR-0026

The user picked the rung-2 road: **bundle the real `Slangify` +
`Slang::Tuxic` verbatim and grow the interpreter** — the OO::Monitors /
EXPORTHOW::DECLARE shape. The design is recorded in
[`docs/adr/0026-slang-activation-architecture.md`](../../docs/adr/0026-slang-activation-architecture.md)
(Proposed): compile-time `use` effect for slang activation, a `$*LANG`
object with `slang_grammar`/`^mixin`/`define_slang` whose interpretation
maps *recognized overridden rule names* onto parser modes (unknown rules =
hard error), and vendoring both modules as batteries. Key measurement
(2026-08-11): `Slang::Tuxic` delegates all registration to `Slangify`'s
EXPORT (`$*LANG.define_slang('MAIN', ...^mixin(...))`); mutsu already runs
`sub EXPORT` with `use`-args, but has no `$*LANG`, no slang API, no parser
mode switch, and no compile-time `use`. Read the ADR before implementing;
the historical options below are kept for context.

## Options (historical — superseded by the decision above)

1. **Build minimal, general slang-switching infrastructure.** The
   architecturally faithful answer, and the one `CLAUDE.md` explicitly
   gestures at ("Keep the parser modular so individual sub-parsers... can be
   extracted and reused in a future architecture"). Large: needs at least a
   defined extension point for what a "slang" can override (which grammar
   productions, expressed how — Rust closures? a limited DSL?), a way for
   `use SomeSlang;` to install one for the rest of compilation, and scoping
   rules for where it applies. Almost certainly an ADR-worthy decision before
   implementation — unblocks more than just `Slang::Tuxic` (any future
   ecosystem module doing the same trick, and mutsu's own future
   user-defined-grammar/slang work referenced in the same CLAUDE.md section).
2. **A narrow, `Slang::Tuxic`-specific parser leniency** (e.g. always accept
   whitespace between a call target and `(`, either unconditionally or gated
   on having seen `use Slang::Tuxic;` earlier in the same compilation unit).
   Cheap, but is exactly the kind of ad-hoc/private-dialect shortcut
   `BATTERIES.md §1` and this project's own "gain vs risk" framework
   (`CLAUDE.md`) warn against: it does not generalize to any OTHER slang
   module the ecosystem might use, and a global unconditional leniency change
   would make mutsu's grammar diverge from real Raku's default grammar (where
   `.method (args)` has different/ambiguous semantics without the slang).
   Do not implement this without discussing the precedent it sets.
3. **Accept `Text::CSV` is out of reach for now and lean on `CSV::Table`
   instead** (see `docs/batteries/csv.md`'s recommendation) — `CSV::Table`
   needs no slang, only the `@0`-in-array-literal parser fix
   (`todo/tickets/numbered-capture-array-var-in-array-literal.md`). Revisit
   `Text::CSV` only once/if slang-switching architecture gets built for
   other reasons (grammars/user-defined slangs are already a known future
   need per the same CLAUDE.md section).

## Verification (once a path is chosen)

- `mutsu -I lib -e 'use Text::CSV; say "ok"'` should print `ok`.
- Re-run `Text::CSV`'s own suite (33 files, 22697 assertions,
  `docs/batteries/csv.md` has the fetch/`-I` setup) — expect it to need
  further, unrelated fixes past the parse barrier, same as `CSV::Table` did.
- If option 1 is chosen, this ticket's scope is really "design and land the
  slang-switching foundation" — `Text::CSV` becomes the first real-world
  consumer/test case for it, not the whole of the work.
