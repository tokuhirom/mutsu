# A Proxy assigned from a called block is FETCHed before the type check

`$typed = $proxy` inside a block that is called (`run({ $c = $p })`, a stored
closure, `lives-ok { $row = ... }`) type-checked the `Proxy` itself and died with
"expected Foo but got Proxy". The same assignment as a statement already FETCHed the
Proxy first (ADR-0040's store boundary). The captured variable has no local slot in
the callee, so the write runs the `SetGlobal` arm, which lacked that step; it now
applies `fetch_proxy_for_store` the way `SetLocal` does. A `:=` bind and a
parameter-raw bind keep the Proxy, so `$p = 1` through a bound Proxy still calls
`STORE`.

This is the `lives-ok { $row = TestDuration.^create(...) }` idiom in Red's
`t/11-types.rakutest`, whose `Model.^create` hands back a Proxy-held value.
