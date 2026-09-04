# An `our proto` no longer hides behind the lexical-shadow exemption

`our` installs a routine in the *package*, not in the lexical scope. A second
`our proto` for the same name is therefore a genuine redeclaration however
deeply nested its block is, and raku refuses it at compile time:

```
$ raku -e 'our proto sub foo($){*}; our multi sub foo(Int $x){"o"};
           { our proto sub foo($){*}; our multi sub foo(Int $x){"i"}; say foo(1); }; say foo(1);'
===SORRY!=== Redeclaration of routine 'foo' (already defined in package GLOBAL).
```

mutsu accepted it and lexically shadowed the outer declaration, printing
`i` then `o`. The divergence was permissive — mutsu ran code raku rejects — so
nothing observable was wrong until someone relied on it.

## Where it came from

`register_proto_decl` used to raise `X::Redeclaration` unconditionally whenever
`functions`/`proto_subs` already held the fully-qualified key. That was too
strict: it also rejected a *lexically* shadowing `proto` in an inner block or a
routine body, which raku allows. The exemption added for that
(`news/2026-09/proto-lexical-shadowing.md`) is keyed on lexical shadowing and
did not distinguish `our`.

The exemption is now gated on `!is_our`. For an `our`-scoped declaration the
redeclaration check is the right answer, and it fires at every nesting level —
a nested block, a routine body, or a `class`/`module` body that resolves to the
same package key.

## What deliberately keeps working

The neighbouring shapes were re-measured against raku v2026.06 and all still
agree:

- `{ our proto foo($){*}; our multi foo(Int){...} }` with **no** outer
  `our proto` — legal, and still accepted.
- A bare `our multi` with no our-scoped proto — still rejected.
- A `my`-scoped inner `proto`/`multi` shadowing an outer `our` one — that *is*
  a lexical declaration, so it still shadows and the outer family still comes
  back when the block exits.
- Two `our proto`s in different packages (`GLOBAL::foo` and `M::foo`) — different
  keys, so no collision.

Pinned as section 10 of `t/multi-proto-lexical-scope.t`, which passes verbatim
under both `mutsu` and `raku`.
