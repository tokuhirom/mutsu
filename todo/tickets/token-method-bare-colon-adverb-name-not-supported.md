# `token`/`rule`/`method` declarations named `name:adverb` (bare identifier, not `:sym<...>`) misparse

## Symptom

Raku lets a multi-dispatch `token`/`rule`/`regex`/`method` variant be named
with a **bare identifier** after the colon, not just the usual `:sym<literal>`
form:

```raku
grammar G {
    token TOP { <gap>+ }
    proto token gap {*}
    token gap:spacer { \s }
    token gap:comment { '#' \N* }
}
say G.parse("  ##comment") ?? "matched" !! "no match";
```

Under `raku`: `matched`. Under mutsu (`target/debug/mutsu`):

```
No such method 'gap' for invocant of type 'Match'
  in block <unit> at ... line 7
```

The same bare-adverb form on a plain `method` (not inside a grammar)
misparses even more visibly — the body appears to execute **immediately at
class-composition time** instead of being registered as a method:

```raku
class Foo {
    method bar:common ($x) {
        say "common: $x";
    }
}
say "parsed ok";
```

mutsu prints `Use of Nil in string context` + `common: ` (the body ran with
`$x` bound to `Nil`) before `parsed ok`; raku prints only `parsed ok` (the
method is registered, not called, since nothing calls `.bar:common` here).

## Root cause (not yet traced into the parser internals)

mutsu's declaration parser only recognizes the `NAME:sym<LITERAL>` spelling
for a colon-qualified multi/token name (used pervasively for grammar
alternation dispatch and operator overloading, e.g. `token sym<+> {...}` /
`method infix:<+> (...)`). The **bare-identifier** adverb form
(`NAME:ADVERB`, no angle brackets, `ADVERB` itself just another identifier)
is a distinct, also-legal spelling that Raku's real grammar accepts — see
`raku-doc/doc/Language/grammars.rakudoc` (`token gap:sym<...>` is documented
but confirm whether the bare-identifier spelling is documented explicitly, or
grep Rakudo's own `Grammar.nqp`/`Actions.nqp` for `token gap:comment`-style
uses; **CBOR::Simple** and other vendored grammars use the `:sym<>` form more
than the bare form, so this specific spelling may be under-exercised in
mutsu's own test suite). Confirm with `raku --target=ast` for both spellings
before implementing, and check `src/parser/stmt/sub/` (method/token
declaration parsing) for where `:sym<...>` is special-cased — the bare form
likely needs the same treatment.

## Why this matters

This is the actual root cause behind **all 17 upstream test files failing**
for the `Config::TOML` battery-slot survey (see `docs/batteries/toml.md`).
`Config::TOML::Parser::Grammar` and `Config::TOML::Parser::Actions` (from
`raku-community-modules/Config-TOML`, the winning candidate for mutsu's TOML
parser slot) declare **48** `method NAME:ADVERB (...)` action methods and a
matching set of `token NAME:ADVERB {...}` grammar alternatives, entirely in
the bare-identifier style (`token string:basic {...}`, `token gap:spacer
{...}`, `method string-basic-char:common (...)`, ...). None of them parse
correctly, which is why the whole module fails to load/run under mutsu even
though every individual TOML-parsing rule is otherwise unremarkable.

This is a general grammar/OO parsing gap, not specific to TOML — any grammar
or Actions class using this idiom (a common, natural way to name alternation
variants when there's no obvious short literal to hang a `:sym<>` off) will
hit the same wall.

## Discovered via

The TOML battery-slot survey (`docs/batteries/toml.md`, 2026-08-22): the
`Config::TOML` candidate scored best on license/maintainer-org/dependents but
failed 0/17 under mutsu. Bisected the failure from the confusing top-level
symptom (`expected statement ... at <file>:28`, where 28 is a line number
*inside `Config/TOML/Parser/Actions.rakumod`*, not the user's script — a
separate line-number-misattribution wrinkle worth noting but not the root
cause) down to the two minimal repros above.

## Next steps

1. Confirm the exact accepted grammar for a colon-qualified declaration name
   in real Raku (`raku --target=ast`) — is the adverb allowed to be *any*
   bare identifier, or only specific known adverbs?
2. Find where mutsu's parser handles `:sym<...>` for `token`/`rule`/`regex`/
   `method` declarations and extend it to also accept a bare identifier after
   the colon.
3. Re-run this file's two repros, then re-run the `Config::TOML` /
   `Crane` survey (`docs/batteries/toml.md`'s worked commands) to see how much
   of the 0/17 clears.
