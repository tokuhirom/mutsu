# A bare `Type:D`/`Type:U` term loses its definiteness constraint entirely

Discovered via the doc-diff harness on `raku-doc/doc/Type/Metamodel/DefiniteHOW.rakudoc`
(around lines 18, 27, 80).

## Root cause

Raku models a definiteness-constrained type (`Any:D`, `Any:U`) as a distinct type object
produced by `Metamodel::DefiniteHOW` wrapping the base type's metaclass — it has its own
`.^name` (`Any:D`), participates correctly in `~~` smart-matching against defined/undefined
values, and `.^base_type` recovers the original unconstrained type.

mutsu's type-smiley (`:D`/`:U`/`:_`) handling (`strip_type_smiley` and the many call sites
using `strip_suffix(":D")`/`strip_suffix(":U")`) is entirely string-based and scoped to
*signature/attribute type-constraint checking* (parameter binding, `where`, attribute
declarations). When `Any:D` appears as a **bare term** (not inside a signature or attribute
declaration), the smiley is dropped somewhere in parsing/evaluation and the expression just
evaluates to the plain type object `Any` — indistinguishable from `Any:U`/`Any:_`. There is no
`DefiniteHOW`-equivalent Value representation anywhere in the codebase (`grep -rn
"DefiniteHOW" src/` finds nothing).

## Minimal repro

```raku
say Any:D.^name; say Any:U.^name; say Any:_.^name;
say Any ~~ Any:D;     say Any ~~ Any:U;
say Any.new ~~ Any:D; say Any.new ~~ Any:U;
say Any:D.^base_type.^name;
```

- `raku`:
  ```
  Any:D
  Any:U
  Any
  False
  True
  True
  False
  Any
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  Any
  Any
  Any
  True
  True
  True
  True
  ```
  and `Any:D.^base_type.^name` throws:
  `No such method 'base_type' for invocant of type 'Perl6::Metamodel::ClassHOW'`

So every bare `Type:D`/`Type:U` term currently behaves exactly like the plain `Type`, both for
`.^name` and for `~~` smart-matching (which always returns `True` regardless of whether the
smiley is `:D` or `:U`, and regardless of whether the tested value is defined or not).

## Why this is a deep/design item, not a shallow fix

Fixing this correctly needs a Value-level representation of "a type object plus a definiteness
constraint" (mirroring Rakudo's `DefiniteHOW`) that:
- is produced when the parser/compiler sees a bare `Type:D`/`Type:U` term (today the smiley is
  only threaded through the signature/attribute-declaration compiler paths, not general
  expression parsing/compilation)
- reports the right `.^name` (`Foo:D`) and `.HOW` (a `DefiniteHOW`-like metaclass, distinct from
  the plain `ClassHOW` `Any:D.^base_type` currently fails on)
- participates correctly in `~~` (`ACCEPTS`) against both defined and undefined values
- exposes `.^base_type` to recover the unconstrained type

This touches the parser (bare-term smiley parsing), the type-object representation, `~~`
dispatch, and the ClassHOW/metamodel reflection surface — a genuine cross-cutting design
decision (how mutsu represents constrained type objects generally), not a single-site patch.
Given the codebase's existing string-suffix-based smiley handling is deeply embedded in the
signature-checking paths, this likely needs its own small ADR to decide the representation
(e.g. a new `Value` variant/wrapper vs. reusing the existing type-object representation with an
extra definiteness tag) before implementation.

## Affected files (starting point)

- `src/runtime/types.rs` (`strip_type_smiley`) — the existing string-based smiley model
- Parser: wherever a bare type-name term (`Any`, `Int`, etc.) is parsed as an expression — needs
  to also recognize a trailing `:D`/`:U`/`:_` smiley outside of signature/attribute-declaration
  context
- `src/runtime/methods_classhow_dispatch.rs` — `.^name`/`.^base_type` and other ClassHOW
  reflection methods, to add `DefiniteHOW`-equivalent behavior
- Smartmatch (`~~`/`ACCEPTS`) dispatch — needs to check both the base type and the definiteness
  flag when the RHS is a definiteness-constrained type object
