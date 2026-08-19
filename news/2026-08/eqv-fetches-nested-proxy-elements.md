# `eqv` now FETCHes a `Proxy` nested inside an Array/List/Hash/Pair

`Value::eqv` is a pure, interpreter-free comparison with no `&mut Interpreter`
access, so it cannot call a `Proxy` element's `FETCH` callback itself.
`eval_binary_with_junctions` already auto-FETCHed a *top-level* `Proxy`
operand before dispatching to `eqv`, but a `Proxy` nested one level down —
inside an Array/List/Hash/Pair, e.g. `(1, 2).map({ Proxy.new(...) }).List` —
passed through untouched, so `$got eqv $expected` compared the raw `Proxy`
objects instead of their fetched values and always returned `False`.

This is exactly what the real, vendored `Test.rakumod`'s `is-deeply` reduces
to (`_is_deeply(Mu $got, Mu $expected) { $got eqv $expected }`), so
`is-deeply` on a list of Proxy-produced values (the URI::Query read-only
Proxy-list shape) always failed under `MUTSU_REAL_TEST=1`, closing
`t/proxy-list-transparency.t`'s last failing assertion.

Fixed in `exec_eqv_op` (`src/vm/vm_comparison_order_ops.rs`) by deep-resolving
Proxies in both operands (the existing `resolve_proxies_in_value` helper,
already used for `t/`'s native-Test-provider argument path) before comparing
— a cheap no-allocation scan in the common Proxy-free case, and a real FETCH
of every nested Proxy otherwise. Pin:
`t/eqv-fetches-nested-proxy-elements.t`, verified byte-for-byte against
`raku`.

Part of the ongoing `todo/deep/vendor-real-test-module.md` campaign
(vendoring rakudo's real `Test.rakumod`); its `t/` residue is down to 5 files.
