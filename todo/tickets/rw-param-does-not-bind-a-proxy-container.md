# `sub f($x is rw)` binds a Proxy's FETCHed value instead of the Proxy container

## The divergence

An `is rw` / `is raw` parameter binds the caller's **container**. When that container is a `Proxy`,
writing the parameter must fire the Proxy's `STORE`:

```
$ raku  -e 'my $n=5; my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); sub f($x is rw) { $x = 42 }; f($p); say $n'
42
$ mutsu -e '...same...'
5
```

`is raw` behaves identically in raku (`42`) and identically wrongly in mutsu (`5`), and so does the
method form (`class C { method m($x is rw) { $x = 42 } }`).

## Root cause

Call arguments are auto-FETCHed unconditionally on the way in:
`vm_call_func_ops.rs:983` and `vm_call_exec_ops.rs:69,215` call `auto_fetch_proxy_args` for every
non-lvalue call, with a small hardcoded `skip_proxy_fetch` name list (`return`, `die`, `fail`,
`leave`, the `__mutsu_*_lvalue` helpers). The FETCH therefore happens **before** signature binding,
so by the time the callee's `is rw` parameter is bound there is no container left to bind — the
Proxy is already gone.

That is the right default for an ordinary read-only parameter (`sub f($x) { }` must see the value,
and that half matches raku today). The gate is simply in the wrong place: whether to FETCH an
argument is a property of the *parameter it binds to*, not of the callee's name.

## Why it is not trivial

Moving the FETCH after binding means the argument-preparation path has to know the target
routine's signature, which it deliberately does not today (the same `auto_fetch_proxy_args` call
serves multi-dispatch, where the candidate is not chosen yet). A narrower version — keep the eager
FETCH, but pass the *unfetched* value alongside so an `is rw`/`is raw` parameter can bind the
container — needs a place to carry it.

Related: `todo/tickets/element-bind-fetches-the-proxy-it-should-install.md` is the same mechanism
seen from the `:=` side, and would likely be fixed by the same change to the skip decision.

## Reproduce

The one-liner above, no fixtures. Confirmed pre-existing (reproduces on `main` at 65fd9dcc6, before
the container-store FETCH boundary landed).
