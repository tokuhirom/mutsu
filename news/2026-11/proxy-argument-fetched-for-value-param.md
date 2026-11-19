# A Proxy passed to a plain parameter is FETCHed when it binds

```raku
my $p := Proxy.new(FETCH => method () { -9 }, STORE => method ($) { });
sub f($x) { abs($x) }; say f($p);   # was "Unknown function: abs", now 9
```

A Proxy held in a variable reaches a call inside that variable's shared cell.
Three binders let it through to a plain `$x` parameter unfetched: the
call-site FETCH (`auto_fetch_proxy_args_for_callee`) and the light-path
admission looked only for a bare or `VarRef`-tagged Proxy, and the TRIR entry
bound whatever the caller's slot held. So the parameter was the Proxy itself
(`$x.VAR.^name` was `Proxy`), and a built-in the body handed it to declined
it. All three now see a Proxy in a cell: the call site FETCHes it for a value
parameter, the light path and the TRIR entry leave the call to the general
binder. An `is rw`, `is raw` or sigilless parameter still binds the Proxy
(#18099).
