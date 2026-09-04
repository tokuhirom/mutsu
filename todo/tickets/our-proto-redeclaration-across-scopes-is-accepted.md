# A nested `our proto` that redeclares a package-scoped one is accepted, where raku rejects it

`our` installs a routine in the *package*, not the lexical scope, so a second
`our proto` for the same name is a genuine redeclaration even when it sits in an
inner block. raku refuses it at compile time; mutsu accepts it and lexically
shadows the outer one:

```
raku  -e 'our proto sub foo($){*}; our multi sub foo(Int $x){"o"}; { our proto sub foo($){*}; our multi sub foo(Int $x){"i"}; say foo(1); }; say foo(1);'
# ===SORRY!=== Redeclaration of routine 'foo' (already defined in package GLOBAL).

mutsu -e '<same>'
# i
# o
```

Measured 2026-09-04 on `main` (2189ac011) against `raku` v2026.06.

This is a *permissive* divergence — mutsu runs code raku rejects — so nothing
observable is wrong until someone relies on it. It is narrow and low priority,
but it should not be lost.

## Where it comes from

`register_proto_decl` used to raise `X::Redeclaration` unconditionally whenever
`functions`/`proto_subs` already held the fully-qualified key. That was too
strict: it also rejected a *lexically* shadowing `proto` in an inner block or a
routine body, which raku allows (fixed 2026-09-04,
`news/2026-09/proto-lexical-shadowing.md` — the `my`-scoped cases now match raku
and are pinned in `t/multi-proto-lexical-scope.t`).

The exemption added there is keyed on lexical shadowing and does not
distinguish `our`. For an `our proto` the exemption should not apply: the
declaration targets the package, so the redeclaration check is the right answer.
The valid neighbouring shapes must keep working — `{ our proto foo($){*}; our
multi foo(Int){...} }` with **no** outer `our proto` is legal and is pinned by
that test file's section 8, as is a bare `our multi` with no our-scoped proto
still being rejected.

## Suggested fix

In the lexical-shadow exemption inside `register_proto_decl`, require the
declaration not to be `our`-scoped (the `__our_scoped` custom trait marks it) —
then re-check the whole of `t/multi-proto-lexical-scope.t` and the shapes above
against raku, since the `our` and `my` paths share the check.
