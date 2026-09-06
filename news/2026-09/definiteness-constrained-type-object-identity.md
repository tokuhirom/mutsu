# `Any:D` keeps its smiley, and a constrained type object reports a `DefiniteHOW`

`todo/deep/definiteness-constrained-type-object-identity-lost.md` claimed that a bare
`Type:D` / `Type:U` term "loses its definiteness constraint entirely", and asked for an ADR
choosing a new `Value` representation for a constrained type object — a new variant or wrapper
holding "base type + definiteness", mirroring Rakudo's `Metamodel::DefiniteHOW`.

Re-measuring first (CLAUDE.md's standing rule, and the right call again here) showed the finding
had generalized from one poisoned example. A 23-row probe run against rakudo and a fresh mutsu
build found that `Int:D`, `Str:D`, `Mu:D`, `Cool:D`, a user `class C`, a `role R` and a `subset`
all *already* worked: correct `.^name`, correct `~~` in both directions, correct `.ACCEPTS`,
`.gist`, `.raku`, `.WHAT`, signature binding, `my Type:D $x`, multi dispatch, and survival
through a variable and a list. mutsu already represents a constrained type object as a
`Value::Package` whose interned `Symbol` carries the smiley, minted by `OpCode::GetBareWord` via
`is_type_with_smiley()` and consumed by the existing `strip_type_smiley` machinery.

Exactly three things were actually wrong.

**`Any` and `Nil` are the two term keywords that also name real types.** The parser's
`keyword_literal()` table matches them as term constants, and `try_kw()`'s word-boundary check
rejects only a following alphanumeric, `_`, or kebab `-` — not a `:`. So `Any:D` matched the
`Any` keyword and the leftover `:D` was swallowed downstream as an adverb, yielding
`Literal(Package("Any"))`; `Nil:D` became a `Pair` outright. Every other type name reached the
identifier path, which handles the smiley correctly. The ticket's repro used `Any`, so it saw a
total failure where the general case was fine. Fixed by rejecting the keyword match when a bare
`:D`/`:U`/`:_` follows, so those two names fall through to the identifier path like every other
type. (`:Do` is still an adverb — a smiley is exactly one character.)

**`.HOW` reported `ClassHOW` for a constrained type object.** `dispatch_how()` now mints
`Perl6::Metamodel::DefiniteHOW` when the type name carries `:D` or `:U`, matching Rakudo.
`:_` asserts nothing and folds back to the plain type, so it keeps its `ClassHOW`.

**`.^base_type` and `.^definite` did not exist.** They are `DefiniteHOW`-only metamethods in
Rakudo, so the new `src/runtime/methods_definitehow.rs` answers `base_type` with the
smiley-stripped type and `definite` with `1`/`0`, and throws `X::Method::NotFound` on an
unconstrained type — which is exactly what Rakudo does, where neither method is on `ClassHOW`.

[ADR-0069](../../docs/adr/0069-a-definiteness-constrained-type-object-is-a-named-type-object.md)
records the representation decision the ticket asked for, and decides it the *other* way from the
ticket's premise: a definiteness-constrained type object stays an ordinary type object whose name
carries the smiley. A new `Value` variant would have forced all ~40 `strip_type_smiley` call
sites to learn a second encoding (or to stringify back into the first), which is the dual-mechanism
shape CLAUDE.md counts as risk rather than gain — and it would need a boxed NaN-box kind for
something an interned symbol already expresses losslessly. The ADR also records the divergences
deliberately left alone: `Int:D.^mro` / `.^methods` / `.^parents` answer off the base type where
Rakudo throws, `Empty:D` and `True:D` are term-constant corners, and
`Metamodel::DefiniteHOW.^name` reports the short name — a pre-existing general `Metamodel::*`
aliasing gap that `Metamodel::ClassHOW` shares.

`t/definite-type-object.t` pins all 74 measured rows — including the ones that already agreed, so
a future refactor of the smiley model cannot silently regress them. The whole file passes verbatim
under rakudo as well as mutsu.
