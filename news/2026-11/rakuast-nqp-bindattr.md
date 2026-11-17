# `nqp::getattr` / `nqp::bindattr` read and write a RakuAST node's attribute

A CHECK-time pass that rewrites the unit's tree binds a node's attribute
directly. Red's `red-compile` does

```raku
nqp::bindattr(nqp::decont($current), RakuAST::ApplyDottyInfix, '$!left', $new);
```

mutsu ignored the bind (and `nqp::getattr` answered `Nil`): a RakuAST node keeps
its attributes as model fields, which the generic attribute ops never looked
at. Now `src/rakuast/attr_bind.rs` maps the attribute `$!left` to the node's
`left` field (a field the node holds, a declared model field of its class, a
named parameter of its `new`, or the class's positional payload such as
`Literal`'s `$!value`). A read answers the field; a bind replaces it in place,
so every holder of the node, the unit tree `$*CU` included, sees the new child,
and the unit runs the rewritten tree. Both the generic op and the compiled
attribute site share the one implementation. Closes #17327; the regression test
is `t/rakuast/rakuast-nqp-bindattr.t`.
