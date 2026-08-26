# A generic `::T` type parameter is resolved when reporting later type-check failures

Found by the doc-diff harness re-run (`Type/Parameter.rakudoc:306`).

## What was wrong

```raku
sub c(::T $x, T $y, $z) { my T $zz = $z };
try c(4, 5, "six");      given $! { .message.say };
try c("four", 5, "six"); given $! { .message.say };
```

raku:

```
Type check failed in assignment to $zz; expected Int but got Str ("six")
Type check failed in binding to parameter '$y'; expected Str but got Int (5)
```

mutsu:

```
Type check failed in assignment to $zz; expected T but got Str ("six")
Calling c(Str, Int, Str) will never work with declared signature (::T $x, T $y, $z)
  X::TypeCheck::Binding::Parameter: Type check failed for y: expected Str, got Int
```

Two symptoms, which turned out to be two independent bugs rather than one root
cause as the ticket assumed.

## Symptom 1: the message named the capture, not the bound type

`::T` binds to `Int` from `$x = 4`, and the `my T $zz = $z` check is performed
*against* `Int` — it correctly rejects `"six"`. Only the error message used the
raw constraint spelling. The `TypeCheck` opcode now runs the constraint through
`resolved_type_capture_name` before building either the assignment or the
binding error — the same resolution the check itself already performs
internally, and a no-op when no capture of that name is bound.

## Symptom 2: it took the compile-time dispatch path

`X::TypeCheck::Binding::Parameter` was already the right class; the *message*
was wrong twice over.

- **It was wrapped in a compile-time shape.** `enhance_binding_error` models
  rakudo's compile-time `Calling f(Str) will never work with declared
  signature (...)` SORRY. But a signature with a generic type capture cannot be
  checked at compile time at all — what `T` means is only known once `$x` binds
  — so rakudo reports a plain *runtime* binding failure for it. The wrapper now
  bows out for such signatures, joining the `Constraint type check failed` and
  `X::Parameter::RW` cases already exempted there.

  The function *had* a `has_type_captures` guard already (used to avoid
  reclassifying the error as the compile-time `X::TypeCheck::Argument`), but it
  looked for a capture among the parameter *names*. mutsu records `::T $x` as
  `ParamDef { name: "x", type_constraint: Some("::T") }`, so that predicate had
  never matched this shape at all. The new check reads the right field.

- **The inner text was mutsu's own.** The parameter type-check failure in
  `runtime/types/binding_signature.rs` hand-rolled
  `X::TypeCheck::Binding::Parameter: Type check failed for y: expected Str, got
  Int`. `RuntimeError::typecheck_binding_parameter_with_repr` already existed
  for exactly this — its doc comment even claims to match "the hand-rolled
  format used throughout `binding_signature.rs`" — with rakudo's wording, the
  sigil restored, `<anon>` for an anonymous parameter, and no `"X::...: "`
  prefix on `.message` (rakudo has none). The site now uses it.

  That also corrected `.got`: the hand-rolled path stored the got TYPE NAME as
  a string, while rakudo's `X::TypeCheck.got` is the offending VALUE
  (`"hello"`, not `"Str"`). `t/exception-types.t` had encoded the old behaviour
  as `got => /Str/`; that subtest failed under `raku t/exception-types.t` too,
  so the local test was corrected to raku's real shape (`expected => Int`,
  `got => "hello"`) per the "roast/raku is authoritative" rule.

Both messages now match `raku` verbatim.
