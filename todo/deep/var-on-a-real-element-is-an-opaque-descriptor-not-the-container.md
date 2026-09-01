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

## Three smaller siblings found in the same measurement

These are independent and could each be a ticket:

- **Multi-dim subscript.** `my @sh[2;2]; @sh[0;0].VAR.^name` is `Scalar` in
  raku, `Array` in mutsu — the `@a[i;j]` subscript never reaches
  `compile_expr_method_var_on_index` (`src/compiler/expr_method.rs`), so the
  element-`.VAR` path is not entered at all.
- **`is default`.** `my @nat is default(0) = 1,2; @nat[0].VAR.default` is `0`
  in raku, `(Any)` in mutsu — `builtin_index_var_meta` reads
  `var_type_constraint` for the default but never the variable's `is default`
  trait.
- **Native arrays.** `my int @ints = 1,2; @ints[0].VAR.^name` is `IntPosRef`
  in raku, `Scalar` in mutsu. A native array's element "container" is a
  positional ref type per element type (`IntPosRef`/`NumPosRef`/…), which mutsu
  has no representation for.
- **Slice subscripts.** `my @a = 1,2; @a[0,1].VAR.^name` is `List` in raku
  (a slice hands back a `List` of containers, and `.VAR` on a `List` is
  identity), `Scalar` in mutsu -- `compile_expr_method_var_on_index` routes
  every subscript to the element-descriptor path, slices included, and a
  slice's `List` result is indistinguishable at runtime from an element that
  happens to hold a `List`. The compiler is the only place that knows, so this
  one is a compile-side gate rather than part of the descriptor rework.

## Repro

```
raku   tmp/var24f.raku
mutsu  tmp/var24f.raku
```

(recreate `tmp/var24f.raku` from the table above; `sub p($l, &c) { say "$l = ",
(try c()).gist }`).
