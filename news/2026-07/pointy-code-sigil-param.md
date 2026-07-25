# `given $code -> &f { … }` no longer dies with X::Assignment::RO

A pointy block's `&`-sigil parameter on `given` / `with` died before running a
single statement:

```raku
my $c = { say "ran" };
given $c -> &to-run { to-run() }   # Cannot modify an immutable value
```

`for ($c,) -> &g { g() }` and `sub s(&f) { f() }` were fine — only the
`given`/`with` forms failed.

## Root cause

`given`/`with` implement their pointy parameter by prepending a head statement
to the body that binds the parameter name to the topic (`pointy_topic_bind`, in
`src/parser/stmt/control.rs`). For an aliasing parameter that head statement is
`&f := $_`, which is exactly the form Raku rejects — "Cannot bind to '&f'
because Code items cannot be rebound" — and mutsu's compiler rejects it too, at
`src/compiler/stmt.rs`, by emitting `OpCode::AssignReadOnly` for any `&name`
assignment whose name is not already a known local. So the desugar produced a
statement the language forbids and the compiler correctly refused it.

`for` was unaffected because it carries the parameter as a real `ParamDef` on
the loop node and binds it through the signature machinery instead.

## Fix

A `&`-sigil pointy parameter now desugars to a *declaration* of that lexical
code alias rather than a rebind. A code alias has no writeback semantics to
lose — the `:=` form exists so that `given @a -> @p { @p.push(1) }` aliases the
source container — so a declaration is the exact shape for it. `$`, `@` and `%`
parameters keep the `:=` head statement unchanged.

Pin: `t/pointy-code-param.t`.

## Found on the way

The pin deliberately does not assert that the alias stops shadowing at the
closing brace: a `given` / `with` body is not a lexical scope for `my` at all in
mutsu, so `given 1 { my $z = 5 }` clobbers an outer `$z` and `given 5 -> $x { }`
leaks `$x` just the same. That is a separate, pre-existing, sigil-blind gap,
now recorded as PLAN 8.22.
