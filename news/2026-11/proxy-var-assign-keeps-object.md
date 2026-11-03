# `my $v = $proxy.VAR` keeps the Proxy object

`.VAR` of a `Proxy` is the Proxy itself, but assigning it to a scalar FETCHed
it, so `$v` held the fetched value and `$v.^name` said `Int`
([#15893](https://github.com/tokuhirom/mutsu/issues/15893)). The Proxy payload
now carries a `var_object` flag that only `.VAR` sets (an `is rw` return or an
assignment result is a decontainerized Proxy too, and must still FETCH), and the
scalar-assignment store boundary (`fetch_proxy_for_scalar_store`) lands such a
Proxy as a value in a `Scalar` wrapper: no FETCH, `.^name` is `Proxy`, and a
later `$v = 5` replaces it instead of calling STORE. Every other store boundary
(array element, hash value, operand, argument) is unchanged.

An earlier attempt that returned the bare decontainerized Proxy from the store
made the slot a *bound* container (`$v = 5` called STORE); one that wrapped every
`.VAR` result broke method calls on the result. Left in #15893: `@a[1] = $q.VAR`
FETCHes in Rakudo but `%h<k> = $q.VAR` does not, and mutsu FETCHes for both; the
sink `$q.VAR;` and `my $b := $q.VAR` also still FETCH.
Regression test: `t/vm/writeback/proxy-var-assign-keeps-object.t`.
