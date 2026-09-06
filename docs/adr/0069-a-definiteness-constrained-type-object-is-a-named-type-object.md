# ADR-0069: A definiteness-constrained type object is a named type object, not a new value kind

- Status: Accepted (implemented)
- Date: 2026-09-07

## Context

`todo/deep/definiteness-constrained-type-object-identity-lost.md` recorded that a bare
`Type:D` / `Type:U` term "loses its definiteness constraint entirely", and asked for an ADR
choosing between two representations for a *constrained type object*:

1. a new `Value` variant (or wrapper) holding "base type + definiteness", mirroring Rakudo's
   `Metamodel::DefiniteHOW`-produced type object, or
2. reusing the existing type-object representation with an extra definiteness tag.

The ticket's evidence was a single measured row set:

```raku
say Any:D.^name; say Any:U.^name; say Any:_.^name;
say Any ~~ Any:D;     say Any ~~ Any:U;
say Any.new ~~ Any:D; say Any.new ~~ Any:U;
say Any:D.^base_type.^name;
```

on which mutsu answered `Any` for every `.^name`, `True` for every `~~`, and threw
`X::Method::NotFound` on `.^base_type`.

### Re-measurement (2026-09-07) — the ticket generalized from one poisoned example

Re-running the ticket's rows plus a much wider probe (user classes, roles, subsets, `Int`,
`Str`, `Mu`, `Cool`, signature constraints, `my Type:D $x`, multi dispatch, storing the
constrained type in a variable, `.gist`/`.raku`/`.WHAT`/`.defined`/`.ACCEPTS`) shows the
ticket's headline is **false for every type except `Any` and `Nil`**:

| probe | raku | mutsu (before) |
| --- | --- | --- |
| `Int:D.^name` | `Int:D` | `Int:D` |
| `Str:D.^name` | `Str:D` | `Str:D` |
| `Mu:D.^name` / `Cool:D.^name` | `Mu:D` / `Cool:D` | `Mu:D` / `Cool:D` |
| `C:D.^name` (user class) | `C:D` | `C:D` |
| `R:D.^name` (role) | `R:D` | `R:D` |
| `Sm:D.^name` (subset) | `Sm:D` | `Sm:D` |
| `42 ~~ Int:D` / `Int ~~ Int:D` | `True` / `False` | `True` / `False` |
| `42 ~~ Int:U` / `Int ~~ Int:U` | `False` / `True` | `False` / `True` |
| `Int:D.ACCEPTS(42)` / `(Int)` | `True` / `False` | `True` / `False` |
| `Int:D.gist` / `.raku` / `.WHAT.^name` | `(Int:D)` / `Int:D` / `Int:D` | same |
| `my $t = Int:D; Int ~~ $t` | `False` | `False` |
| `multi m(Int:D)` / `multi m(Int:U)` | `D` / `U` | `D` / `U` |
| **`Any:D.^name`** | **`Any:D`** | **`Any`** |
| **`Any ~~ Any:D`** | **`False`** | **`True`** |
| **`Nil:D.^name`** | **`Nil:D`** | **`Pair`** |
| **`Int:D.HOW.^name`** | **`Perl6::Metamodel::DefiniteHOW`** | **`Perl6::Metamodel::ClassHOW`** |
| **`Int:D.^base_type`** | **`Int`** | **NotFound** |
| **`Int:D.^definite`** | **`1`** | **NotFound** |

So mutsu *already* has a working representation of a constrained type object — a
`Value::Package` whose interned `Symbol` carries the smiley (`Package("Int:D")`), produced by
`OpCode::GetBareWord` via `is_type_with_smiley()`, and consumed by the existing string-based
`strip_type_smiley` machinery in `runtime/types/type_matching.rs`, `args_matching.rs`,
`accessors.rs` and friends. `.^name`, `~~`/`ACCEPTS`, `.gist`, `.raku`, `.WHAT`, signature
binding, `my Type:D $x`, and multi dispatch all already observe the smiley through that one
representation.

`Any` and `Nil` diverge for a reason unrelated to the type model: they are the two entries of
the parser's `keyword_literal()` term table that name real types. `try_kw()`'s word-boundary
check rejects only a following alphanumeric / `_` / kebab `-`, so `Any:D` matched the `Any`
keyword and the leftover `:D` was consumed downstream as an adverb — producing
`Literal(Package("Any"))` for `Any:D` and a `Pair` for `Nil:D`. Every other type name reaches
the identifier path, which does handle the smiley. The ticket's repro happened to use `Any`,
and the conclusion "the smiley is dropped somewhere in parsing/evaluation" was generalized from
that single poisoned example.

## Decision

**Keep representation (2): a definiteness-constrained type object is an ordinary type object
whose name carries the smiley.** Do not introduce a new `Value` variant or wrapper.

Concretely:

- `Type:D` / `Type:U` / `Type:_` evaluates to `Value::Package(Symbol("Type:D"))` — the
  representation mutsu already produces for every type name other than `Any`/`Nil`.
- The `strip_type_smiley` string model stays. It is not a workaround being paid down; it *is*
  the chosen encoding, and it is the one every constraint-checking path already reads.
- `Metamodel::DefiniteHOW` is modelled the way mutsu already models `ClassHOW`, `SubsetHOW`,
  `EnumHOW`, `CoercionHOW` and the rest: as a **derived** metaobject that `.HOW` mints from
  the type name, not as stored state. A name carrying a smiley reports `DefiniteHOW`; the
  metaobject answers `base_type` (the name with the smiley stripped) and `definite`
  (`1` for `:D`, `0` for `:U`).

### Why not a new `Value` variant

Costed against the measured evidence, a new variant is all cost and no gain:

- Every one of the ~40 `strip_type_smiley` call sites listed in the ticket would have to learn a
  second encoding, or the new variant would have to stringify back into the old one at each —
  i.e. the string model would survive anyway, with a second model layered on top. That is the
  "dual mechanism" shape CLAUDE.md's gain/risk section names as *risk*, not gain.
- `Value` is NaN-boxed (ADR-0005). A "base type + definiteness" payload does not fit an inline
  payload, so the variant would need a boxed/`Gc` kind — a new GC-visible kind for what is
  losslessly expressible as an interned symbol.
- The measurements show it buys nothing: 20 of 23 previously-suspect rows already agree under
  the name-carried encoding. The three that do not are a parser word-boundary bug and two
  missing metamethods — neither is a representation problem.

`OpCode` and `Value` size guards are untouched by this decision: no opcode payload and no
`Value` variant changes.

### Deliberately out of scope

Rakudo's `DefiniteHOW` has a *smaller* method set than `ClassHOW`: `Int:D.^mro`,
`Int:D.^methods` and `Int:D.^parents` all throw `X::Method::NotFound` there, while mutsu answers
them off the base type. Tightening that would mean gating the whole MOP surface on the HOW kind
for a case no measured consumer exercises, so it is recorded as a known divergence rather than
implemented. Likewise `Any:D.new` (rakudo: "You cannot create an instance of this type
(Any:D)"), `Empty:D` (rakudo: `Slip:D`) and `True:D` (rakudo: `(Bool)`), which are term-constant
corners of the `keyword_literal` table rather than type-object behaviour, and
`Metamodel::DefiniteHOW.^name` reporting the short name — a pre-existing, general
`Metamodel::*` aliasing gap that `Metamodel::ClassHOW` shares.

## Consequences

- The fix is three narrow changes rather than a representation campaign: a parser word-boundary
  guard, a `.HOW` branch, and two metamethods.
- `todo/deep/`'s framing of this as a cross-cutting design item is retired. Any future work on
  constrained type objects starts from "the smiley is part of the type's name" and adds to the
  `strip_type_smiley` oracle, rather than proposing a parallel value model.
- Pinned by `t/definite-type-object.t`, which covers every row of the table above — including
  the rows that already agreed, so a future refactor cannot silently regress them.
