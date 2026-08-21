# `token`/`method` variants named with a bare-identifier adverb

Raku lets a multi-dispatch `token`/`rule`/`regex`/`method` variant be named
with a **bare identifier** after the colon, not just the familiar
`:sym<literal>` spelling:

```raku
grammar G {
    token TOP { <gap>+ }
    proto token gap {*}
    token gap:spacer  { \s }
    token gap:comment { '#' \N* }
}
```

mutsu recognized only `NAME:sym<...>` (and its `NAME:<...>` shorthand). The
bare form failed in three separate places, all of which are now fixed:

1. **Declaration parsing.** `parse_sub_name_inner` consumed a colon adverb only
   when an angle/guillemet-bracketed value followed it, so `method bar:common
   ($x) {...}` stopped its name at `bar` and left `:common ($x) {...}` behind
   as loose source — which mutsu then *executed* at class-composition time
   instead of registering a method. (Grammar `token` declarations went through
   `parse_token_like_name`, which already accepted the bare spelling, so only
   the `sub`/`method` side needed the parser change. Operator-category bases
   (`infix`, `prefix`, …) are excluded, so `X::Syntax::Extension::Category`
   diagnostics are unchanged.)
2. **Proto-variant resolution.** `is_proto_variant_suffix` matched only
   `:sym<`/`:sym«`/`:<`/`:«`, so `gap:spacer` was never collected as a
   candidate of proto `gap` and the parse failed with "No such method 'gap'".
   It now accepts any adverb-introducing colon (`::` is still a package
   separator, never an adverb), and `extract_variant_ident` gives a bare adverb
   its own candidate identity.
3. **Action dispatch.** The winning-variant marker records only the adverb's
   *value* (`spacer`), which both spellings share, and three duplicated sites
   in `invoke_grammar_actions` unconditionally rebuilt the action-method name as
   `rule:sym<value>`. They now share one helper that asks the actions class
   which spelling it actually declares, so `method gap:spacer ($/)` is found.

Two further gaps surfaced on the way and are fixed here too:

- **`my Array[Str:D] @k`** was rejected with "Invalid type smiley ':D]'". The
  smiley scan used `rfind(':')` over the whole constraint, so it mistook the
  *inner* type's smiley inside a parameterisation for the outer type's. It now
  looks only at bracket-depth zero, and the declaration and signature paths
  share one implementation instead of two subtly different copies.
- **An object hash never bound to a typed `%h` parameter.** `my Bool:D
  %k{Array:D}` stores its constraint as the composite string `Bool:D{Array:D}`,
  and `typed_container_param_matches` compared that whole string against the
  parameter's `Associative[Bool:D]`. Only the value type takes part in that
  check; the key type is a separate constraint the parameter does not name.

## Why this came up

This is the entire blocker recorded in `docs/batteries/toml.md` for the TOML
battery slot: `Config::TOML::Parser::Grammar` and `::Actions` name all ~48 of
their alternatives in the bare style (`token string:basic`, `method
string-basic-char:common`, …), so the whole distribution failed to load. It now
loads, and its upstream suite runs — reaching real per-assertion failures
instead of 19 files that could not parse. The remaining gaps are filed
separately (`todo/deep/is-rw-lvalue-return-is-caller-side-ast-reinterpretation.md`
is the large one; `todo/tickets/push-with-slip-arg-in-sink-context.md` and
`todo/tickets/config-toml-remaining-suite-gaps.md` cover the rest).

The parsing gap is general, not TOML-specific: any grammar or actions class
using this idiom — a natural way to name alternatives when there is no obvious
short literal to hang a `:sym<>` off — hit the same wall. Pinned by
`t/bare-adverb-declaration-name.t`.
