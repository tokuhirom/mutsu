# `@a[0] := $proxy` installs the `Proxy` as the element's container

ADR-0040 §9 rules that a `:=` bind is *outside* the store boundary that FETCHes
a `Proxy` — the bind installs the container itself. The element spelling did not
honour that:

```
my $n = 5;
my @a = (1, 2);
@a[0] := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
$n = 9;
say @a[0];              # raku: 9        mutsu (before): 5
say @a[0].VAR.^name;    # raku: Proxy    mutsu (before): Scalar
@a[0] = 20;
say $n;                 # raku: 20       mutsu (before): 9  (write dropped)
```

The scalar form (`my $p := Proxy.new(...)`) was already right; only the element
lost the container, and it lost it in three separate places.

## Root cause, in three parts

**1. The bind's argument was FETCHed before the bind machinery saw it.**
`@a[0] := EXPR` is lowered to the internal marker helper
`__mutsu_bind_index_value`, which is compiled as an ordinary `CallFunc` — so the
call-site argument auto-FETCH ran on it. Three sibling `__mutsu_*_lvalue`
helpers were exempt; this one had never been added. The `Proxy` was therefore
already gone by the time the element-assign path recognised the bind marker,
and the element got a snapshot of the fetched value.

**2. A store to such an element replaced it.** Even with the `Proxy` installed,
`@a[0] = 20` overwrote the element instead of firing its `STORE`. Every
element-assign path — the shared-var fast paths, the plain-hash fast path, the
slow path — ends in a plain `items_mut()[i] = ...` / `insert(k, v)`, so none of
them could see that the destination was a container that mediates its own store.

**3. `.VAR` described a container that was not there.** `element_var_meta`
synthesized a `Scalar` descriptor unconditionally, and the `.VAR` lowerings
(`__mutsu_index_var_meta` / `__mutsu_anon_index_var_meta`) are `CallFunc`s too,
so they were handed the FETCHed value rather than the container they exist to
describe.

## The fix

Each part is fixed at the site that owns the decision, not by widening a
downstream fallback:

- `callee_takes_arg_containers` — the documented replacement for the old
  hardcoded `skip_proxy_fetch` list — now names the bind and `.VAR` lowerings
  alongside the assign/delete ones, with the contract spelled out: these are not
  user routines, they are the desugaring of lvalue/bind/introspection syntax and
  each is *given* the container it is about to write through, install, or
  describe.
- The destination-side `Proxy` check sits **above** the element-assign dispatch,
  in `exec_index_assign_expr_named_op_seeded_inner` — the same "one hook above
  the fast paths" shape ADR-0040 slice 1 used for itemization. When the element
  currently holds a `Proxy` (and this is not itself a `:=` bind), the store fires
  that `Proxy`'s `STORE` and returns. Only the one shape a `Proxy` element can
  be reached by is handled — a plain `@`/`%` container under a simple `Int`/`Str`
  subscript — and every other shape falls through untouched.
- `element_var_meta` returns the element's own container when that container is
  a `Proxy`, as a decontainerized `Proxy` (`Value::proxy_var_object`, the same
  representation the scalar `.VAR` path already produced) so `.WHAT`/`.^name`
  answer `Proxy` instead of FETCHing through to the held value.

The store boundary itself is unchanged, and a regression row pins that: a
`Proxy` *assigned* into an element (`@a[0] = Proxy.new(...)`) still FETCHes on
the way in and leaves an ordinary `Scalar` element. Only `:=` installs a
container.

Pinned by `t/proxy-binds-container-not-value.t`, whose array and hash blocks
cover live tracking, `.VAR`, the store-through, and the assigned-not-bound
counter-row.
