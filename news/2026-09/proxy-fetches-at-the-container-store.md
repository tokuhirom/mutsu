# A `Proxy` now FETCHes at the container store, and the `for`-loop landmine is gone

Raku reads the RHS of `=` in value context, so a `Proxy` that lands *inside* a container — a `$`
`Scalar`, an `Array` element, a `Hash` value, an attribute — is FETCHed on the way in and what
lands is a plain value. mutsu stored the `Proxy` itself and re-FETCHed it on every read. That is
invisible until the Proxy's backing lexical changes:

```
$ raku  -e 'my $n = 5; my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); say @a.raku'
[5]
$ mutsu -e '...same...'            # before
[Proxy]
```

## Why it was Tier S

The stored-Proxy model needed a compensator, and mutsu put it in the `for` loop:
`exec_for_loop_body` auto-FETCHed a `Proxy` item for a non-`is rw` loop and (ADR-0045 §5 Q6) left
it alone for an `is rw` one. That compensation **flipped on an unrelated same-named lexical
anywhere in the compilation unit**:

```raku
{ my $x = 1; }                               # add ONLY this line
{
    my $n = 5;
    my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    for @a -> $x is rw { $x = 42 }
    say "n=$n";                              # raku 5, mutsu 42  <-- STORE fired
}
```

So an `is rw` loop over what should have been a plain array element silently wrote through the
Proxy's `STORE` and corrupted an outer variable — a wrong write with nothing detecting it. It was
enough of a landmine that `t/for-loop-element-alias.t`'s `.kv` row had to name its parameters
`$p`/`$q` instead of the natural `$x`/`$y`, with a comment pointing at the ticket.

## The fix: the same boundary ADR-0040 already established

ADR-0040 decided that an `Array`/`Hash` element is itemized **at the store**, not compensated at
the read. The Proxy question has the same shape and the same answer, so it is now the same
boundary, one call earlier — `Interpreter::fetch_proxy_for_store` and its element-wise twin
`fetch_proxy_container_elements` (discriminated by `ArrayKind` exactly like
`itemize_real_array_elements`) run wherever ADR-0040's itemization runs:

- the scalar and `@`/`%` variable stores (`exec_set_local_op_inner`, `vm_var_assign_local.rs`);
- the element, chained, deep-chained, multi-dim and computed-target index assigns;
- the `[...]` / `{...}` / `%(...)` construction ops, so a literal FETCHes its elements;
- `push` / `unshift` / `append` / `prepend` / `splice`, including the `@a."$name"(...)` dynamic
  dispatch, and reaching through a `Pair` for `%h.push(k => $p)`;
- the `state $s = …` initializer and the rw-accessor writeback
  (`__mutsu_assign_method_lvalue`, which is exempt from the caller's argument auto-FETCH because
  its *target* must keep its container — but its assigned *value* is an ordinary rvalue).

Two things stay outside the boundary, deliberately: a `:=` bind installs the `Proxy` itself
(`$p.VAR.^name` is still `Proxy`, and `$p = 1` still fires `STORE`), and a `List`'s elements are
not containers, so `my $l = (1, $p, 3)` keeps its Proxy and re-FETCHes on every read — the same
§1.6 discriminator ADR-0040 already uses, unchanged.

The `for`-loop carve-out in `vm_for_loop_body.rs` stays, but it is no longer a compensator for a
mis-stored element: it now only serves a Proxy that reaches a loop as a **List** item
(`for $proxy-list.list { }`), which is exactly what `t/proxy-list-transparency.t` pins.

## Fallout

`substr-rw` returns a `Proxy`, so this also fixed a straightforwardly wrong answer:

```
my $str = "hello"; my $s = substr-rw($str, 0, 2); $s = "XY"; say $str;
# raku: hello    mutsu before: XYny)    mutsu now: hello
```

`t/for-loop-element-alias.t`'s workaround parameters went back to `$x`/`$y`, and the file now
declares the once-fatal `my $x` on purpose, immediately above the Q6 rows it used to break.

## Pins

`t/proxy-store-boundary.t` — 28 rows, dual-oracled against `raku` (every store shape above, the
`:=` exemption, and the `substr-rw` consequence). Recorded in ADR-0040 §9.

## Left open

Three narrower divergences in the same area were measured and filed rather than folded in, since
none is a store:

- `todo/tickets/list-element-proxy-not-rendered-through-fetch.md` — the read side of the `List`
  exemption: `say (1, $p, 3)` must FETCH at stringification.
- `todo/tickets/rw-param-does-not-bind-a-proxy-container.md` — `sub f($x is rw)` should bind the
  Proxy, not its FETCHed value.
- `todo/tickets/element-bind-fetches-the-proxy-it-should-install.md` — `@a[0] := $p` FETCHes where
  raku installs the container.
