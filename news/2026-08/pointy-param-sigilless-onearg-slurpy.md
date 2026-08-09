# Pointy-block signatures accept the sigilless one-arg slurpy `+a`

`-> +a { ... }` and `-> $x, +a { ... }` (the sigilless single-argument-rule
slurpy parameter) failed to parse in pointy-block ("arrow") signatures,
dying with `X::Syntax::Malformed: Malformed initializer`. The sub-signature
parser already supported this form (`sub f($x, +a) { ... }`); only the
pointy-block parser was missing it.

## Root cause

`parse_pointy_param` (`src/parser/stmt/control/pointy_param.rs`) handled
`**`-double-slurpy and `*`/`+` followed by a sigil (`@ % $ &`), but had no
branch for a bare `+` followed by an identifier. Control fell through to
`var_name`, which requires a sigil and fails on `+`, so the whole
pointy-block parse failed and the surrounding statement parser surfaced a
generic "Malformed initializer" / "Confused" error far from the real
offending line.

## Fix

Added a sigilless-slurpy branch to `parse_pointy_param`, mirroring the
existing handling in the sub-signature parser
(`src/parser/stmt/sub_param/param_inner.rs`): parse the identifier, any
`is` traits, and an optional default, then return a `ParamDef` with
`slurpy: true, onearg: true, sigilless: true`. The lambda body compiler
already supports sigilless params via `ParamDef.sigilless`, so no further
plumbing was needed.

## Verification

- `-> +a { a }`, `-> $x, +a { a }`, and the Cro shape `-> 'lit', +a { a }`
  now bind correctly (verified against `raku` — values match; the
  Array-vs-List `.raku` gist difference is a pre-existing cosmetic gap
  shared with the sub-signature path, out of scope here).
- `t/http-router-named-urls.t` (vendored Cro::HTTP suite) gets past this
  parse failure and progresses to its next (separately tracked) blocker,
  a missing `Parameter.constraint_list` introspection method.
- New pin: `t/pointy-onearg-slurpy.t` (passes under both `mutsu` and
  `raku`).
- All 43 whitelisted `S06-signature`/slurpy roast files (1071 subtests)
  and the full `make test` suite pass with no regressions.
