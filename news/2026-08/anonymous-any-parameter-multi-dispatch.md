# Anonymous `Any` parameters participate in multi dispatch

An anonymous type-only `Any` parameter, such as `multi f(Any)`, previously
matched no non-`Nil` argument. This made the idiomatic `Any` fallback candidate
unreachable, including through `callsame` redispatch.

The parser represents both anonymous type constraints and bare value terms with
the placeholder parameter name `__type_only__`. Multi-dispatch argument matching
therefore resolves bare value terms from the environment before comparing them
with the argument. `Any` is also installed in that environment as a sentinel,
so it was mistaken for a value term and compared with `Nil` instead of being
used as a nominal type constraint.

Argument matching now uses the same discrimination as signature binding: an
anonymous constraint is compared with an environment value only when
`is_resolvable_type` says it is not a type. Resolvable names such as `Any`,
`Cool`, and user-defined types continue through ordinary type matching, while
bare enum-value parameters such as `Less` retain value-based dispatch.

The regression test `t/anonymous-any-multi-dispatch.t` covers direct `Int` and
`Str` arguments, `callsame` into an `Any` fallback, nominal-type ordering, and
the enum-value behavior that shares the parser representation.
