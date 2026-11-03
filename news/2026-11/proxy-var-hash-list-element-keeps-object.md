# `.VAR` of a Proxy stays the Proxy object in a hash value or list element

Slice 3 of [#15893](https://github.com/tokuhirom/mutsu/issues/15893).
`%h<k> = $q.VAR` and `my @l = ($q.VAR,)` FETCHed the Proxy that `.VAR` returns;
Rakudo keeps the object (`Proxy`, no FETCH). A hash-value store and the
element-wise list store now use `fetch_proxy_for_scalar_store`, which lands a
`.VAR` object as a value; an array element (`@a[1] = $q.VAR`) still FETCHes,
which is what Rakudo does too, and every plain Proxy still FETCHes on store.

Left in #15893: `my $b := $q.VAR; $b = 5` calls STORE where Rakudo dies with
"Cannot assign to a readonly variable".
Regression test: `t/collections/proxy-var-hash-list-element-keeps-object.t`.
