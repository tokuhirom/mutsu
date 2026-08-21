# ADR-0026: Slang activation — bundle Slangify + Slang::Tuxic verbatim, map recognized grammar-mixin overrides onto parser modes

- Status: Accepted, implemented (approved 2026-08-11; the full campaign —
  §2.1 → §2.2 → §2.3 → §2.4, in dependency order — landed as four PRs across
  2026-08-11/12; see "Outcome" below)
- Date: 2026-08-11
- Deciders: tokuhirom, Claude
- Related: [BATTERIES.md](../../BATTERIES.md) §1 (rung 2: grow the interpreter
  until the real module runs verbatim; native provision banned 2026-08-01),
  `news/2026-08/exporthow-declare-mop.md` (the OO::Monitors precedent this ADR
  follows), CLAUDE.md "Raku's context-dependent parsing (slangs)",
  [docs/batteries/csv.md](../batteries/csv.md) (the CSV campaign this unblocks)
- Addresses: `todo/deep/text-csv-needs-slang-tuxic-support.md`

## Outcome (2026-08-12)

The campaign landed as four PRs across two days
(`news/2026-08/slang-activation-machinery.md`):

- **#6273** — parser slang modes (§2.3): unit-scoped `SlangModes` state gating
  spaced identifier calls and spaced methodops, plus the recognized-override
  map (rule name → mode, unknown rule → hard error).
- **#6274** — a regex scanner fix (prerequisite): a character class nested in
  a lookaround desynced quote/angle tracking, which blocked Tuxic's own
  `methodop` token from parsing at all. General fix, rakudo-verified.
- **#6275** — Slangify-style inner `&EXPORT` (prerequisite, §1.2): an
  `&EXPORT` a module imports from another module's EXPORT map becomes that
  module's own EXPORT for its importers, matching Slangify's registration
  mechanism.
- The activation slice (§2.1 + §2.2 + §2.4): `use X` where X's source
  directly `use`s Slangify runs X's whole load in a fresh interpreter on a
  fresh thread with a compile-time `$*LANG` bound; Slangify's inner EXPORT
  runs verbatim and `define_slang` maps the roles' declared token names onto
  the §2.3 parser modes.

The upstream `Slang-Tuxic` test suite passes 8/8 (gated), and this unblocked
the Text::CSV battery (`docs/batteries/csv.md`, completed 2026-08-13). Slangify's
own upstream test activates a fixture slang overriding `identifier`/`name` —
those rules were not initially in the map and failed loudly per this ADR's
design; since resolved (`news/2026-08/slang-piersing-identifier-name-overrides.md`).

## 1. Context

`Text::CSV` — the deepest-tested CSV candidate (33 files / 22697 assertions
under raku) and the only remaining blocker in the CSV battery campaign — has
`use Slang::Tuxic;` at the top of `Text::CSV.rakumod` itself, so it cannot
even parse on mutsu. The user decision (2026-08-11) is to take the BATTERIES
rung-2 road: **bundle the real modules and grow the interpreter until they
run verbatim**, the same shape as the EXPORTHOW::DECLARE campaign that made
OO::Monitors run as a bundled battery. Hand-patching the vendored Text::CSV
(~500 spaced call sites) is rejected — it violates vendor-verbatim and
re-breaks on every upstream bump.

### 1.1 What the modules actually are (measured 2026-08-11)

The dependency chain is two tiny modules whose weight lives in Rakudo
compiler internals:

- **`Slang::Tuxic` (0.0.5, 111 lines)** defines two roles — `Tuxic` (RakuAST
  grammar) and `Tuxic::Legacy` (NQP grammar) — overriding three grammar
  rules: `routine-declarator:sym<sub>` / `routine_declarator:sym<sub>`,
  `term:sym<identifier>`, and `methodop`. The token bodies are written
  against Rakudo-internal rules and hooks (`<.unspace>`, `<args>`,
  `<longname>`, `$*QSIGIL`, `$*DOTTY`, `$*W.is_type`, `self.add_mystery`,
  `HLL::Compiler.lineof`). Its last line delegates all registration:
  `use Slangify Tuxic, Mu, Tuxic::Legacy, Mu;`
- **`Slangify` (0.0.4, 47 lines)** is an EXPORT generator: its outer
  `sub EXPORT($grammar, $actions?, $legacy-grammar?, $legacy-actions?)`
  returns a Map exporting an inner `&EXPORT` that, at the *user's* `use`
  time, reads the compiling language object and installs the mixin:

  ```raku
  $*LANG.define_slang('MAIN',
    $*LANG.slang_grammar('MAIN').^mixin($grammar<>),
    $*LANG.slang_actions('MAIN').^mixin(...));
  ```

### 1.2 What mutsu has and lacks (measured)

Present already:

- `sub EXPORT` execution on module load (`runtime_module_export_sub.rs`),
  including `use Module ARGS` argument passing (the `use Slangify Tuxic, Mu,
  Tuxic::Legacy, Mu;` shape parses today).
- A regex engine and user grammar support (token/rule declarators) — the
  role *definitions* in Slang::Tuxic parse as data.

Missing:

- `$*LANG` is `Nil`; no language object, no `define_slang` /
  `slang_grammar` / `slang_actions` anywhere in the codebase.
- The parser is a hand-written Rust recursive descent with **no notion of a
  parse-mode switch**, and `use` has **no compile-time effect**: a module's
  EXPORT runs when the (already fully parsed) program executes, which is too
  late — `Text::CSV.rakumod` fails to *parse* before any code runs.

## 2. Decision

Three pieces, in dependency order. The guiding rule: **the module code runs
verbatim; the *interpretation* of "a role was mixed into the MAIN grammar"
is a mutsu-native mapping from the overridden rule names onto
hand-implemented parser modes.** mutsu does not execute the Rakudo-internal
token bodies (see §4 Rejected).

### 2.1 Compile-time `use` effect for slang activation

When the parser encounters a `use` statement whose module turns out to
activate a slang, the rest of that compilation unit must be parsed in the
changed mode. This requires running the used module's EXPORT chain at parse
time (BEGIN-time semantics for `use`), scoped to the current compilation
unit:

- On `use X ...` the parser suspends, loads and executes module `X` (which
  may itself `use Slangify ...`, executing Slangify's returned `&EXPORT`
  immediately), then resumes parsing with whatever slang state the EXPORT
  installed.
- Slang state is **lexically scoped to the compilation unit** (file /
  EVAL string): `Text::CSV.rakumod` parses in Tuxic mode; the user's own
  program that `use Text::CSV` does not.

This is the architecturally load-bearing piece and is more general than
slangs (a genuine compile-time `use` is a standing gap), but this ADR only
commits to the slang-activation slice of it.

### 2.2 The `$*LANG` object and its slang API

A compile-time language object available as `$*LANG` during EXPORT
execution, carrying:

- `slang_grammar('MAIN')` / `slang_actions('MAIN')` — return a handle
  representing mutsu's own MAIN parser (an opaque type object; there is no
  real grammar object to expose).
- `.^mixin(Role)` on that handle — records the role composition, returning
  a new handle carrying the accumulated override set.
- `define_slang('MAIN', $grammar, $actions)` — inspects the handle's
  recorded roles, extracts the **names** of the overridden grammar rules,
  and maps each onto a parser-mode flag (§2.3). An override whose rule name
  is not in the supported map is a **hard compile-time error** naming the
  rule (`X::NYI`-flavoured), never a silent ignore — an unknown override
  means the slang's semantics would be silently wrong.

`Tuxic` vs `Tuxic::Legacy`: Slangify picks by `$*LANG.^name.starts-with('Raku::')`.
mutsu's `$*LANG` will report a non-`Raku::` name, selecting the `::Legacy`
role — either role maps to the same three rule names, so the mode outcome
is identical; the choice only has to be deterministic.

### 2.3 Recognized-override map, seeded with Tuxic's three rules

| Overridden rule | Parser mode behavior |
| --- | --- |
| `term:sym<identifier>` | An identifier followed by `\s*'('` (not `\s*':'`) parses as a call with the parenthesized args — except for the keywords `sub if elsif while until for` and known type names (the token's own exclusion list). |
| `methodop` | `.method (args)` — whitespace between method name and `(` — parses as a method call with args (stock mutsu raises "no space allowed between method name and the left parenthesis" here). |
| `routine-declarator:sym<sub>` / `routine_declarator:sym<sub>` | Effectively stock behavior (the override re-states the standard rule so the spaced form composes with sub declarations); map to a no-op mode unless testing shows otherwise. |

The mode flags live in the parser state, set only while parsing a
compilation unit whose slang state enables them.

### 2.4 Bundling

`Slangify` and `Slang::Tuxic` are vendored as batteries (both Artistic-2.0
class licensing to be confirmed at vendoring time per selection-method.md),
so `use Slang::Tuxic;` resolves to the real upstream source with zero
config. `Text::CSV` itself is bundled only after its suite is measured
green — it stays the campaign's yardstick, not part of this ADR's scope.

## 3. Consequences

- `mutsu -I lib -e 'use Text::CSV; say "ok"'` prints `ok` (the ticket's
  verification gate); the Text::CSV suite then becomes measurable, and —
  per the CSV::Table precedent — will surface further unrelated interpreter
  bugs to fix as ordinary rung-2 work.
- Any future ecosystem slang built on Slangify gets the registration
  surface for free; only its override rules need adding to the map (or it
  fails loudly with the unsupported-rule error).
- The compile-time-`use` slice (§2.1) is the first real step toward the
  "parser modes / sub-language switching" future CLAUDE.md already gestures
  at for user-defined grammars.
- Cost honesty: §2.1 touches the parser/module-loader boundary and is the
  riskiest slice; §2.2/§2.3 are contained. Text::CSV full-green after the
  parse barrier is expected to be a multi-week campaign in total.

## 4. Alternatives considered (rejected)

- **Execute the mixin token bodies as the parser** (full grammar-driven
  faithfulness): requires rewriting mutsu's hand-written parser as a
  Raku-grammar interpreter with Rakudo-compatible internal rule names,
  dynamic hooks (`$*W`, `add_mystery`, cursor protocol) — a different
  project, out of all proportion to any current need.
- **Unconditional / global whitespace leniency**: makes mutsu's default
  grammar diverge from Raku's (private dialect) — exactly what BATTERIES.md
  §1 forbids.
- **Parse-time hardcoding on the module NAME `Slang::Tuxic`** (flip modes
  when the literal `use Slang::Tuxic;` is seen, without running anything):
  cheap, but it is name-keyed native provision in disguise — the bundled
  module would be dead code, and a second Slangify-based slang would need
  another hardcode. The chosen design keys on what the module *does*
  (which rules its roles override), keeping the module load-bearing.
- **Hand-patching vendored Text::CSV**: rejected in §1.

## 5. Open questions

- §2.1 scoping details: does slang state need to survive `EVAL` boundaries
  inside the unit, and how does it interact with the precompilation-less
  module cache?
- Whether `routine-declarator:sym<sub>` truly maps to a no-op on mutsu
  (decide empirically once Text::CSV parses).
- Whether the `$*LANG` handle should also stub `slang_actions` mixins as
  recorded-but-inert (Tuxic passes `Mu` for actions, so nothing is needed
  today).
