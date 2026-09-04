# `@a[0] := $proxy` FETCHes the Proxy instead of installing it as the element's container

## The divergence

A `:=` bind installs the container itself, so binding a `Proxy` to an element must leave that
element live:

```
$ raku  -e 'my $n=5; my @a = (1,2); @a[0] := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); $n=9; say @a[0]'
9
$ mutsu -e '...same...'
5
```

mutsu snapshots the FETCHed value at bind time, so the element stops tracking. The scalar form
(`my $p := Proxy.new(...)`) is correct — it is only the *element* bind that loses the container.

## Root cause

`@a[0] := EXPR` is lowered to a call to the internal helper `__mutsu_bind_index_value` (which wraps
the value in a marker `Pair` for `IndexAssignExprNamed` to recognise) — see the bytecode:

```
CallFunc { name_idx: "__mutsu_bind_index_value", arity: 2 }
IndexAssignExprNamed { name_idx: "@a", is_positional: true, ... }
```

That is an ordinary `CallFunc`, and `vm_call_func_ops.rs:972` auto-FETCHes every argument of a
non-lvalue call unless the callee is on a hardcoded `skip_proxy_fetch` list. Three sibling helpers
are on that list (`__mutsu_assign_method_lvalue`, `__mutsu_index_assign_method_lvalue`,
`__mutsu_index_delete_method_lvalue`); `__mutsu_bind_index_value` was never added, so the Proxy is
FETCHed before the bind machinery ever sees it.

## Why it is not just "add the name to the list"

Adding it is the obvious first experiment and may well be right — the element-assign path already
recognises the marker `Pair` and skips its own store-side Proxy FETCH for it
(`exec_index_assign_expr_named_op_seeded_inner`, `exec_index_assign_expr_nested_op`'s
`is_bind_value`). But that list is a name-keyed workaround for a decision that belongs to the
*parameter*, not the callee — see `todo/tickets/rw-param-does-not-bind-a-proxy-container.md`, which
is the same mechanism from the signature side. Prefer fixing the decision once over lengthening the
list a fourth time, and check what an element bound to a live Proxy does to the ADR-0036 element-cell
machinery (`@a[0]` promotion, `.VAR`, `:p` pair producers) before calling it done.

## Reproduce

The one-liner above, no fixtures. Confirmed pre-existing (reproduces on `main` at 65fd9dcc6).
