# A pointy block with one literal parameter keeps its constraint

`-> 'about' { … }` reported an unconstrained `Any` parameter, while
`-> 'company', 'careers' { … }` was correct:

```raku
say (-> 'about' { }).signature;              # was :($__literal__), now :(Str)
say (-> 'about' { }).signature.params[0].constraints.ACCEPTS('about');  # was False
```

The parser has two shapes for a pointy block. Two or more parameters build an
`Expr::AnonSubParams` carrying full `ParamDef`s; exactly one parameter takes a
name-only `Expr::Lambda` fast path whenever the parameter is "simple". That
`simple_single` gate already excluded traits, shaped constraints, named/slurpy/
optional forms, defaults, type constraints, `where`, sub-signatures and `@_`/
`%_` — but not `literal_value`, which the `Lambda` form has nowhere to put. So
a single literal parameter silently lost its constraint. Adding
`first.literal_value.is_none()` to the gate routes it through `AnonSubParams`
like every other non-simple parameter.

Pinned by `t/pointy-single-literal-param.t`.

This was the second of the two bugs that kept `Cro::HTTP`'s router from
compiling its route matchers: `Cro::HTTP::Router` builds a route's URL segment
matcher from `$param.constraints`, so every single-segment route
(`get -> 'product' { … }`) compiled to a matcher with no segment at all.

The related gap that a literal parameter is not *enforced* at bind time
(`(-> 'about' { })('nope')` still runs the body — in Rakudo it throws
`X::TypeCheck::Binding::Parameter`) is recorded in
`todo/tickets/literal-parameters-are-not-enforced-at-bind.md`; it is a
dispatch-wide question, not specific to this path.
