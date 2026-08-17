# Slang activation: support the `identifier`/`name` rule overrides (Slangify's Piersing fixture)

The ADR-0026 slang activation machinery runs the bundled `Slangify` +
`Slang::Tuxic` verbatim, but the recognized-override map
(`src/parser/stmt/simple/slang_modes.rs`, `apply_slang_rule_override`) only
covered Tuxic's three rules. `Slangify`'s own upstream test
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
called as `pass? "Successfully slanged"`. This previously failed with `Slang
activation NYI: grammar rule override 'identifier' is not supported by this
implementation`.

## What landed

A third `SlangModes` flag, `ident_trailing_punct`, mapped from both
`"identifier"` and `"name"` rule names. Rather than gating it inside the
shared low-level identifier scanner (`parse_raku_ident`) — which would have
broken a sigiled variable's own identifier parsing, e.g. `sub f($x?) {...}`
(an *optional signature parameter*) needs `$x` + trailing `?`-marker, not a
variable literally named `x?` — the mode is scoped to the two grammar
productions that actually correspond to reading a bareword `identifier`/
`name` as a term or declarator:

- `identifier_or_call` (`src/parser/primary/ident/identifier_call.rs`) — a
  bareword call/term (`pass? "..."`).
- `parse_sub_name_inner` (`src/parser/stmt/sub/sub_name.rs`) — a sub
  declaration name (`sub pass?(|c) {...}`), skipped for operator-category
  names (`infix:<+>` etc.) where the override has no bearing.

A shared helper, `consume_slang_ident_trailing_punct`, does the actual
consumption: a no-op when the mode is off, and when on, consumes a single
trailing `?`/`!` — but refuses to consume when the same character
immediately repeats, so a compact ternary written with no surrounding
whitespace (`cond??a!!b`) does not lose one half of its `??`/`!!` to the
identifier scan.

**A third parallel identifier scanner surfaced mid-implementation:**
`known_call_stmt` (`src/parser/stmt/simple/control_stmts.rs`), the
statement-level dispatcher for known/imported listop-style calls (`ok "...";`,
`pass "...";`), does its own separate `ident()` scan before checking
`is_known_call`. Without patching it too, `sub pass?(|c) { pass |c }` called
as `pass? "msg"` silently matched the BUILTIN `pass` (stopping at 4 chars,
leaving `? "msg"` for the argument parser to misinterpret) instead of the
user-declared `pass?` sub — reproduced by comparing `pass? "msg"` calling
sites end-to-end against `raku`; both agreed until this third site was found
and patched the same way.

## Verification

- `t/slang-piersing-activation.t` (new, with a `t/lib/Piersing.rakumod`
  fixture mirroring the upstream one) — 6 assertions, byte-identical against
  `raku`.
- `Slangify`'s own `t/01-basic.rakutest` now passes 1/1 (both debug and
  release binaries); added to `batteries-whitelist.txt`.
- `Slang::Tuxic`'s `t/01-basic.rakutest` (8/8) and the existing
  `t/slang-tuxic-activation.t` (11 assertions) still pass — no regression
  from widening the shared `SlangModes`/consumption helper.
- Full local `t/` suite (3198 files) all green.
