# An `is rw` parameter binds the caller's `Proxy` container

`sub f($x is rw) { $x = 42 }` given a `Proxy`-bound argument dropped the write
on the floor, and silently destroyed the caller's binding while doing it:

```
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
sub f($x is rw) { $x = 42 }
f($p);
say $n;              # raku: 42   mutsu (before): 5
say $p.VAR.^name;    # raku: Proxy  mutsu (before): Scalar
```

`is raw` and the method form (`method m($x is rw)`) were wrong the same way.
Nothing reported anything: exit 0, no warning, the `STORE` never ran.

## Root cause

Two things went wrong, one behind the other.

Call arguments are auto-FETCHed on the way in (`auto_fetch_proxy_args`), gated
by a hardcoded list of *callee names* (`return`, `die`, `fail`, `leave`, the
`__mutsu_*_lvalue` helpers). That is the right default for an ordinary readonly
parameter — `sub f($x) { }` must see the value — but the gate was keyed on the
wrong thing: **whether an argument keeps its container is a property of the
parameter it binds to, not of the callee's name.** A callee-name list cannot
express "this argument, because this parameter is `is rw`".

The FETCH alone would only have cost the write. What destroyed the binding was
the second half: a scalar `is rw`/`is raw` parameter aliases its caller through
a shared `ContainerRef` cell, and the cell installer
(`bind_function_args_values`, the `rw_shared_cell_key` arm) reused the caller's
cell only when the caller's variable already held a `ContainerRef`. A `Proxy` is
not one, so it boxed the *already FETCHed* value into a fresh cell and installed
that cell under the caller's name — replacing the `Proxy` with a plain
container. `$p.VAR.^name` went `Proxy` -> `Scalar`, and every later `$p = ...`
stopped firing `STORE` too.

## The fix

Decide it where the parameter is known. The same `rw_shared_cell_key` arm now
checks the caller's live container first: when it is a `Proxy`, the parameter
binds *that `Proxy`*, with no cell and no write to the caller's entry. Everything
downstream then already works — the body's `$x = 42` reaches the
`Proxy`-holding-local store path (`exec_set_local_op`) and fires `STORE`, reads
of `$x` FETCH through the existing value-context hooks, and the exit writeback
sees the same `Proxy` it bound and leaves the caller alone.

Type checking is unaffected: `check_and_coerce_param_type` still runs on the
FETCHed *value*, as it must — `sub f(Int $x is rw)` constrains what the `Proxy`
holds, not the `Proxy`.

ADR-0040 §9 stated this boundary from the store side ("a `Proxy` is FETCHed when
it lands inside a container"); an `is rw` parameter is the other side of it — it
binds a container rather than storing into one — so this is the rule §9 already
implies, applied at the site that knows the parameter.

The call-site name list survives for what it is actually good for, and is now a
documented predicate (`callee_takes_arg_containers`) naming its two closed
categories: the control-flow builtins that hand a value straight back out
(`return`/`return-rw`/`leave`/`die`/`fail`), and the compiler lowerings of
lvalue/bind/introspection syntax, which are *given* the container they are about
to write through, install, or describe.

Pinned by `t/proxy-binds-container-not-value.t` (24 rows, dual-oracled against
`raku` v2026.06), which covers `is rw`, `is raw`, the method form, the
read-modify-write shape, and the readonly-parameter row that must keep FETCHing.
