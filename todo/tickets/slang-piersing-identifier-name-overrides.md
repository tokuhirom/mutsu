# Slang activation: support the `identifier` / `name` rule overrides (Slangify's Piersing fixture)

The ADR-0026 slang activation machinery runs the bundled `Slangify` +
`Slang::Tuxic` verbatim, but the recognized-override map
(`src/parser/stmt/simple/slang_modes.rs`, `apply_slang_rule_override`) only
covers Tuxic's three rules. `Slangify`'s own upstream test
(`t/01-basic.rakutest`) activates a fixture slang, `t/Piersing.rakumod`,
whose role overrides two other rules:

```raku
my role Piersing {
    token identifier { <ident> ... [ <.apostrophe> <.ident> ]* <[?!]>? }
    token name       { [ | <identifier> <morename>* | <morename>+ ] <[?!]>? }
}
use Slangify Piersing, Mu;
```

i.e. identifiers (and names) may end with `?` or `!`: `sub pass?(|c) { ... }`
called as `pass? "Successfully slanged"`.

Today this fails loudly and correctly per the ADR:

```
$ mutsu -I <slangify>/lib -I <slangify>/t <that test>
Runtime error: slang activation for 'Piersing' failed: Slang activation NYI:
grammar rule override 'identifier' is not supported by this implementation
```

so the file is NOT in `batteries-whitelist.txt` (Slangify carries no gated
tests; Slang::Tuxic's 8/8 file is the gate coverage for the machinery).

## What implementing it takes

A third parser mode (`ident_trailing_punct`) in `SlangModes`, mapped from
both rule names, plus the actual grammar change: identifier lexing must
accept a trailing `?`/`!` when the mode is on — that touches
`parse_ident_with_hyphens` (or a mode-gated wrapper at its call sites) for
both declaration sites (`sub pass?(...)`) and call sites, without breaking
`$x?` ternaries or `!` prefix parsing in the stock grammar. The mode is off
everywhere except units that activated such a slang, so the blast radius is
contained, but the call sites of identifier parsing are many — survey first.

## Verification

- `scripts/battery-testsuite.sh` (or the direct run above) shows Slangify's
  `01-basic.rakutest` passing 1/1; then whitelist it.
- A `t/` pin with a Piersing-style fixture role in `t/lib`.
