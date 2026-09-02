# `.VAR` on a real Array/Hash element is an opaque descriptor, not the element's container

`@a[0].VAR` / `%h<k>.VAR` in raku hands back the element's actual `Scalar`
container, which delegates almost everything to the value it holds. mutsu
synthesizes an opaque `Scalar` *instance* carrying three attributes
(`name`, `dynamic`, `default`) and nothing else, so every method that raku
answers from the contained value answers wrongly.

Measured 2026-09-01 on `main` + ADR-0040 slice 3 (`tmp/var24f.raku`,
`my @real = 1, [3,4]; my %h = a => [1,2]`):

| program | raku | mutsu |
| --- | --- | --- |
| `@real[1].VAR.raku` | `[3, 4]` | `Scalar.new` |
| `@real[1].VAR.gist` | `$[3, 4]` | `Scalar.new` |
| `@real[1].VAR.elems` | `2` | `1` |
| `@real[1].VAR[0]` | `[3 4]` | `(Any)` |
| `@real[1].VAR.Str` | `Array<…>` | `Scalar()` |
| `%h<a>.VAR.raku` | `[1, 2]` | `Scalar.new` |

`.VAR.^name` (`Scalar`), `.VAR.WHAT` (`(Scalar)`), `.VAR.name`, `.VAR.dynamic`
and the ADR-0040 slice-3 container-vs-value discriminator all agree — this is
only about the *contents* of the object `.VAR` returns.

## Why it is deep, not a ticket

The honest fix is for `.VAR` on an element to return the element's real
container — the `ContainerRef` cell ADR-0036 already promotes elements to via
`array_slot_ref` / `hash_slot_ref` — instead of a descriptor built out of
variable metadata. That is the *aliasing* surface
(`docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md`), and it
collides with the read chokepoint that deliberately decontainerizes a
`ContainerRef` on every element read (`resolve_array_entry`,
`src/vm/vm_var_ops.rs`) — `.VAR` would need to become one of the few readers
that does *not* decont, alongside the existing `raku`/`gist`/`perl` exceptions
in `exec_call_method_op_impl`.

The descriptor also carries information the cell does not: `.VAR.name` and
`.VAR.dynamic` are properties of the *variable*, not of the element, so a cell
returned by `array_slot_ref` cannot answer them on its own. Whatever replaces
`builtin_index_var_meta` (`src/runtime/builtins.rs`) has to carry both — which
is a representation decision, i.e. an ADR-shaped question, not a patch.

## The smaller siblings found in the same measurement

Two of the four are **fixed** (2026-09-02,
`news/2026-09/var-on-a-subscript-slice-and-is-default.md`, pinned by
`t/var-on-subscript.t`):

- ~~**`is default`.**~~ `builtin_index_var_meta` consults the container's
  `is default(...)` before falling back to the declared element type.
- ~~**Slice subscripts.**~~ The compiler gates the element-descriptor path on
  `Compiler::index_expr_is_slice`, so `@a[0,1]` / `%h<a b>` / `@a[0..1]` /
  `@a[^2]` / `@a[@i]` compile as ordinary subscripts and `.VAR`'s normal
  identity-on-a-`List` dispatch answers. An index whose *runtime* value happens
  to be a list (`my $i = (0,1); @a[$i]`) still takes the element path — the
  compiler cannot see that, and neither can the runtime.

Two remain:

- **Native arrays.** `my int @ints = 1,2; @ints[0].VAR.^name` is `IntPosRef`
  in raku, `Scalar` in mutsu. A native array's element "container" is a
  positional ref type per element type (`IntPosRef`/`NumPosRef`/…), which mutsu
  has no representation for. (`@ints[0].VAR.default` is `Nil` in raku, `(int)`
  in mutsu — the same gap.)
- **Multi-dim and chained subscripts** — bigger than the original note said.
  Re-measured 2026-09-02:

  | program | raku | mutsu |
  | --- | --- | --- |
  | `my @sh[2;2]; @sh[0;0].VAR.^name` | `Scalar` | `Any` |
  | `@sh[0;0].VAR.name` | `element` | `Nil` |
  | `my %d; %d<a><b>=1; %d<a><b>.VAR.^name` | `Scalar` | `Int` |
  | `%d<a><b>.VAR.name` | `element` | `Nil` |
  | `my @g; @g[0][1]=2; @g[0][1].VAR.name` | `element` | `Nil` |

  raku's answer is **uniform** for every nested element: `Scalar` / name
  `element` / default `(Any)` / dynamic `False` — the inner container is
  anonymous, so `.VAR.name` is NOT the outer variable's name even when the
  outer variable is named (`@sh[0;0]` is `element`, not `@sh`).

  It is not a compiler-routing patch, which is why it did not ship with the two
  above. `MultiDimIndex` and a chained `Index` have no `index_assign_target_name`
  to hand `__mutsu_index_var_meta`, and the builtin's whole job is to decide
  from the *source container* whether the element is a container — for a nested
  subscript that source is the intermediate value (`%d<a>`), which is not in
  the environment under any name. Handing it over would mean compiling the
  parent subscript a second time, which is exactly the double evaluation slice
  3 restructured the hook to avoid. So it belongs with the descriptor rework
  below: whatever replaces `builtin_index_var_meta` has to be able to name the
  parent container it came from.

## Repro

```
raku   tmp/var24f.raku
mutsu  tmp/var24f.raku
```

(recreate `tmp/var24f.raku` from the table above; `sub p($l, &c) { say "$l = ",
(try c()).gist }`).
