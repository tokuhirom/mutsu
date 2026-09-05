# A `Proxy` bound into an element through a variable now mediates its own store

ADR-0040 §9.1 says a store to an element that IS a `Proxy` fires that `Proxy`'s
`STORE`, hooked once above the element-assign dispatch. That held for
`@a[0] := Proxy.new(...)` but not for the far more common spelling — binding a
variable whose own container is the `Proxy`:

```raku
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });

my @a = 1, 2, 3;
@a[1] := $p;
say @a[1].VAR.^name;     # Proxy   -- the bind did install it
@a[1] = 99;
say $n;                  # raku: 99     mutsu: 5      -- STORE never fired
say @a[1].VAR.^name;     # raku: Proxy  mutsu: Scalar -- the Proxy was replaced
```

`%h<k> := $p` was the same gap on the hash side.

## Root cause — the hook was in the right place, reading the wrong depth

The ticket's hypothesis was that the hook "sits above the `List` element-assign
dispatch but not above the `Array` one". Read against ADR-0040 §9.1 and the code,
that is not what happened: the hook is exactly where the ADR says, in
`exec_index_assign_expr_named_op_seeded_inner`
(`src/vm/vm_var_assign_element.rs`), above every fast and slow element-assign
path, and it covers `Array` and `Hash` alike. It also fires correctly for the
`List` spelling *and* for `@a[0] := Proxy.new(...)`.

The real discriminator is the **bind source**, not the destination container.
Under `rust-gdb`, `existing_element_container` returned NaN-box kind 43 —
`ContainerRef` — not `Proxy`:

- `@a[0] := Proxy.new(...)` installs the `Proxy` value itself as the element.
- `@a[0] := $p` binds a *variable*, so it goes through the ordinary element-bind
  aliasing machinery and installs the `ContainerRef` cell that spelling uses for
  any lexical. The `Proxy` is one layer down inside it.

Both mean the same thing in Raku. The read paths already looked through the
cell, which is why `@a[1]` tracked `$n` live and `.VAR.^name` correctly answered
`Proxy` — that agreement is what made the bug look like a pure store-side
failure. Only the destination-side check read the outer layer alone, found a
`ContainerRef` rather than a `Proxy`, and fell through to a store that replaced
the binding.

## The fix

`existing_element_container` now unwraps alias cells (bounded, so a cell cycle
cannot spin) before answering, on both the positional and associative arms, so
both bind spellings find the same mediating container. A non-`Proxy` cell is
unaffected: the unwrapped value simply is not a `Proxy`, and the store falls
through exactly as before — a plain lexical bound into an element still aliases
rather than being mistaken for a store-mediating container.

Verified against real `raku` across 24 probes, including the shapes that must
*not* change: plain lexical aliasing (`@c[0] := $x`), an array bound into an
element, ordinary element stores, nested and multi-dimensional stores, a `Proxy`
as an *rvalue* (still FETCHed at the store, ADR-0040 §9), the `List` element
store, and rebinding an already-bound element.

Pinned by ten new rows in `t/proxy-binds-container-not-value.t` (now 34), which
passes identically under `raku` and mutsu.
