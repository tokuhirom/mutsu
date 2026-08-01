# A reflected Signature keeps its literal parameters, and answers `.ACCEPTS`

Two related gaps around signature-versus-capture matching:

```raku
my $sig = :('greet', $name);
say \('other', 'world') ~~ $sig;   # Rakudo: False    mutsu: True
say $sig.ACCEPTS(\('greet', 'x')); # Rakudo: True     mutsu: X::Method::NotFound
```

A literal parameter constrains the **value**. The parser already records it
(`SigParam::literal_value`) and multi dispatch already honours it — `multi
f('greet', $n)` versus `multi f('bye', $n)` picks correctly — but the code that
matches a *reflected* signature against a capture only checked the parameter's
type constraint, which for `'greet'` is the `Str` the parser inferred from the
literal. So any two-positional capture whose first element was a `Str` matched.

`Signature.ACCEPTS` was missing entirely: the smartmatch arm implemented
`$capture ~~ $signature`, but the explicit method form fell through to
`X::Method::NotFound` (the generic `Mu.ACCEPTS` fallback only backs plain
scalars).

## Fix

- `sig_param_matches_value` rejects a candidate that is not equal to the
  parameter's `literal_value`, before the type check.
- `Signature.ACCEPTS($capture)` dispatches to the existing smartmatch arm.

## Why it matters

This is Cro's route dispatcher. `compile-route` emits a bind check into the
generated path matcher:

```
<?{ my $han = @handlers[0]; $han.signature.ACCEPTS($cap) || !(@*BIND-FAILS.push(...)) }>
```

for any route whose signature has a constraint — which includes every literal
path segment, i.e. every route of the form `get -> 'greet', $name { … }`. Without
`.ACCEPTS` the assertion threw; with a literal-blind `.ACCEPTS` it would have
matched the wrong route.

`t/signature-accepts-literal-param.t` pins both halves with ten assertions,
passing unmodified under Rakudo, including that ordinary dispatch through a
literal parameter is unchanged.
