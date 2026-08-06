# A literal parameter is now enforced when the routine is called

A parameter written as a literal (`sub f("a") { }`, `-> 'about' { }`) constrains
the argument: Rakudo throws `X::TypeCheck::Binding::Parameter` when the argument
is not that literal. mutsu already recorded the literal (`ParamDef::literal_value`)
and reported it correctly through introspection -- `.signature.params[0].constraints`,
`Signature.ACCEPTS`, and multi-dispatch candidate selection all honoured it -- but
the *binder* ignored it, so a direct call bound any argument:

```raku
sub f("a") { "hit" }
say f("a");   # hit   (both)
say f("b");   # raku: X::TypeCheck::Binding::Parameter; mutsu (before): hit
```

Same for a pointy block: `(-> 'about' { })('nope')` used to run the body.

## Fix

`bind_function_args_values` (`src/runtime/types/binding_signature.rs`) is the
shared binder for a direct (non-multi) sub call, a pointy block/closure call, and
the winning candidate of a multi dispatch (after candidate selection has already
run). Added a check, right after the existing `type_constraint` handling in the
positional-binding loop: when a parameter carries a `literal_value` and the bound
value does not equal it, raise a new
`RuntimeError::typecheck_binding_parameter_literal`, matching raku's exact wording
("Constraint type check failed in binding to parameter '<anon>'; expected \"a\" but
got \"b\"") and exposing `.expected`/`.got` as the literal/actual VALUES themselves
(not a type name), with the parameter name always `<anon>` (a literal parameter is
always positional and unnamed in raku's own syntax).

Multi-dispatch candidate selection (`args_matching.rs`) already filters a
non-matching literal candidate *before* the winning candidate reaches this binder,
so the new check only ever fires for a call that has already committed to a
literal-parameter candidate -- it does not change dispatch outcomes, only closes
the correctness gap where a directly-called sub or pointy block silently accepted
a non-matching argument.

Pinned by `t/literal-param-enforced-at-call.t`.
