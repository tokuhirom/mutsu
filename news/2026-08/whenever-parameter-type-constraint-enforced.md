# A `whenever` pointy-block parameter's declared type is now enforced

A `whenever` signature parameter used to accept any value regardless of its
declared type, so a type mismatch surfaced later as a confusing missing-method
error on the wrong type instead of a binding failure at the point of emit:

```raku
my $s = Supplier.new;
my $o = supply { whenever $s -> Int $x { emit $x } };
$o.tap(-> $v { say "out $v" });
$s.emit("str");
# raku:  Type check failed in binding to parameter '$x'; expected Int but got Str ("str")
# mutsu (before): out str
```

The same constraint on a plain `.tap(-> Int $x { ... })` block was already
enforced — the gap was specific to the callback `run_whenever_with_value`
built: the parser's `parse_type_constraint_expr` call consumed the type
constraint just to make the pointy param parse, then discarded it
(`_tc`), and the callback's `Value::make_sub_owning` call passed an empty
`param_defs` list, so the standard positional type-check-and-coerce path in
`bind_function_args_values` never ran for it.

Fixed by threading the type constraint all the way through: `Stmt::Whenever`
gained a `param_type` field, `OpCode::WheneverScope` a `param_type_idx`
constant slot, and `run_whenever_with_value` now builds a `ParamDef` carrying
the constraint for the callback's parameter — the same machinery an ordinary
typed block parameter already uses, so no new type-check logic was needed.

This also fixes `X::TypeCheck::Binding::Parameter`'s `.parameter.named`/`.type`
introspection being unreachable from a `whenever` binding failure, which
`Cro::HTTP::ResponseParser`'s `whenever $in -> Cro::TCP::Message $packet { ... }`
depends on for diagnosing a malformed message instead of a raw `No such method`
error deep inside the handler.

Pin: `t/whenever-typed-pointy-param.t` (extended with two new assertions: the
mismatch dies with `X::TypeCheck::Binding::Parameter`, and a matching value
still binds normally).
