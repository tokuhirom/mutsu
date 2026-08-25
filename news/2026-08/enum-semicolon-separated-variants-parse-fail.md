# `enum`'s parenthesized body now IS the parenthesized term, so `;` separates variants

`enum Foo (A_INET => 0; A_INET6 => 10);` did not parse as an enum at all, and the
multi-line spelling used by `raku-doc/doc/Language/nativecall.rakudoc` (line 1186)
failed the same way. Worse, the failure surfaced as `Undeclared routine: enum
used` — the statement layer backtracked out of the declaration and re-read the
bare word `enum` as an ordinary function call.

## Root cause

`enum`'s `(...)` body had a hand-rolled grammar of its own in
`src/parser/stmt/decl/enum_decl.rs`: `parse_static_enum_variants` walked a
comma-only list of `parse_enum_variant_entry`s, and a "dynamic" fallback re-parsed
the body as one expression optionally followed by a second comma-only loop.
Neither accepted `;`.

Rakudo has no such private rule. An `enum` body is just a **term**, so every
separator the parenthesized term supports works there — which is precisely what
makes `;` legal. mutsu's parenthesized term (`primary/container/paren.rs`) already
implemented that correctly, section splitting and trailing-separator absorption
included; the enum path simply never used it.

## Fix

The `(` branch of both `parse_enum_decl_body_with_type` and
`parse_anon_enum_body` now calls `paren_expr` and then *decomposes* the resulting
expression into variants (`enum_variants_from_body` / `enum_variant_from_expr`),
falling back to the existing `__DYNAMIC__` computed-body representation when any
element is not a plain name or `name => value` pair. The two bespoke variant-list
parsers were deleted rather than taught a second separator, so there is no
second, divergent dialect of the rule to keep in sync.

This makes `;`, `,`, a trailing separator, the single-variant and the multi-line
spellings all work by construction, and it is why `enum Foo (A => 1, B => 2)`,
`enum Foo (R => 1,)` and computed bodies such as `enum Foo (1..3 Z=> <x y z>)`
keep behaving exactly as before — they now travel the same code path as any other
parenthesized term.

## The diagnostic

A body that genuinely fails to parse is now a **fatal** parse error
(`Malformed enum body for enum 'Foo'`) instead of a recoverable one. `enum <Name> (`
is a commit point: once it has been seen there is no honest reading of the text as
a call to a routine named `enum`, and letting the statement layer backtrack turned
every malformed enum body into the useless `Undeclared routine: enum used`.
(The anonymous `enum (...)` form, which has no name to commit on, still fails
recoverably.)

Pinned by `t/paren-semicolon-statement-list.t`.
