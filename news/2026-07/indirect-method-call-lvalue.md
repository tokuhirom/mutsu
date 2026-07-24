# Indirect method-call lvalue assignment writes through the accessor

An indirect (quoted or interpolated) method call used as an assignment target —
`$o."$name"() = value` — now writes back through the object's `is rw` accessor,
exactly like the literal form `$o.attr = value`. Previously it threw
`Cannot modify an immutable value (cannot assign through non-callable value)`.

## Root cause

The assignment lowering distinguished a literal method-call lvalue
(`Expr::MethodCall`, routed to the `__mutsu_assign_method_lvalue` rw-accessor
writeback) from a callable lvalue (`$code() = v`, routed to
`__mutsu_assign_callable_lvalue`). An indirect method call parses to a distinct
`Expr::DynamicMethodCall`, which had no arm in the three assignment-target
dispatchers (`lvalue_assign_to_expr`, `assign_to_target_expr`, and the
expression-position handler in `logic.rs`), so it fell through to the
callable-lvalue path. At runtime the `DynamicMethodCall` was evaluated to the
accessor's *current value* — not a `Callable` — and the writeback raised
"cannot assign through non-callable value".

## Fix

A shared `dynamic_method_lvalue_assign_expr` helper lowers a `DynamicMethodCall`
lvalue to the same `__mutsu_assign_method_lvalue` writeback as the literal form,
but passes the method-name *expression* (which evaluates eagerly to the name
string) instead of a literal. A private indirect call (`$o!"$name"() = v`,
`modifier: Some('!')`) prefixes `!` so it dispatches to the private accessor.
All three dispatch sites gained a `DynamicMethodCall` arm.

## Surfaced by

HTTP::UserAgent's `HTTP::Cookies` grammar action assigns cookie attributes via a
computed accessor name: `$h."{$a<name>.lc}"() = ~$a<value>`. This made
`HTTP::Cookies.set-cookie` abort, and the upstream `t/030-cookies.rakutest`
suite now passes in full. Pinned by `t/indirect-method-call-lvalue.t`.
