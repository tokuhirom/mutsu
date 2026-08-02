# A literal parameter is not enforced when the routine is called

A parameter written as a literal (`sub f("a") { }`, `-> 'about' { }`) constrains
the argument: Rakudo throws `X::TypeCheck::Binding::Parameter` when the argument
is not that literal. mutsu records the literal (`ParamDef::literal_value`) and
reports it correctly through introspection — `.signature.params[0].constraints`,
`Signature.ACCEPTS` and multi-dispatch candidate selection all honour it — but
the *binder* ignores it, so a direct call binds any argument:

```raku
sub f("a") { "hit" }
say f("a");   # hit   (both)
say f("b");   # raku: X::TypeCheck::Binding::Parameter; mutsu: hit
```

Same for a pointy block: `(-> 'about' { })('nope')` runs the body.

## Why it is not a one-liner

The obvious fix — "throw when `literal_value` is set and the argument differs" —
has to be placed so that it does NOT break `multi` dispatch, where a
literal-parameter candidate that does not match must be *skipped* in favour of
the next candidate rather than dying. The binder is shared between the
single-candidate call path and the per-candidate trial bind, so the check needs
a mode (or has to live only on the committed path, after candidate selection).
`proto`/`only` and `where`-constrained parameters raise the same question, so
the fix should cover the whole "argument fails a parameter's constraint at bind
time" family rather than special-casing literals.

## Affected files

- `src/vm/` parameter binding (`bind_params`-family) and
  `src/runtime/` dispatch (`calls.rs` / `dispatch.rs`) for the multi path.
- `ParamDef::literal_value` is set in `src/parser/stmt/sub_param/`.

Introspection is already correct as of
`t/pointy-single-literal-param.t` (which pins the `.signature` half, fixed in
`news/2026-08/pointy-single-literal-parameter.md`); this ticket is only about
enforcement at call time.
