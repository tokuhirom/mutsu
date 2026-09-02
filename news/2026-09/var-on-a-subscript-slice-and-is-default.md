# `.VAR` on a slice, and on a container with `is default(...)`

ADR-0040 slice 3 established the model for `.VAR` on a subscript: which of the
element's *container* and the element *itself* you get is decided by the source
container. Two shapes that model did not reach are fixed here, both split out of
`todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`.

## A slice hands back a `List`, not an element

```
raku  -e 'my @a = 1,2; say @a[0,1].VAR.^name'   # List
mutsu -e 'my @a = 1,2; say @a[0,1].VAR.^name'   # Scalar  (before)
```

In raku a slice subscript hands back a `List` of containers, and `.VAR` on a
`List` is identity — so the answer is `List`. mutsu routed *every* named-container
subscript to the element-descriptor path, slices included.

The runtime cannot tell the difference: mutsu's elements are not real containers,
so a slice's `List` result is indistinguishable from an element that happens to
hold a `List`. The compiler is the only place that knows, so the fix is a
compile-side gate (`Compiler::index_expr_is_slice`) on the statically
recognizable slice spellings — a comma list (`@a[0,1]`, `%h<a b>`), a range
(`@a[0..1]`, `@a[^2]`), and an `@`-sigiled index (`@a[@i]`). Those compile as
ordinary subscripts and let `.VAR`'s normal dispatch answer.

An index whose *runtime* value happens to be a list (`my $i = (0,1); @a[$i]`)
still takes the element path. The compiler cannot see that — and neither can the
runtime, which is the whole reason the gate lives in the compiler.

## `is default(...)` is a property of the container

```
raku  -e 'my @a is default(0) = 1,2; say @a[0].VAR.default'   # 0
mutsu -e 'my @a is default(0) = 1,2; say @a[0].VAR.default'   # (Any)  (before)
```

`builtin_index_var_meta` built the descriptor's `default` from the *variable's
declared type*, which never sees the `is default(...)` trait — that lives on the
container (`ArrayData::default` / `HashData::default`, already read by
`typed_container_default` for missing-key reads). It consults the container
first now, and falls back to the declared element type, else `Any`. Arrays and
hashes both.

## Verification

`t/var-on-subscript.t` pins sixteen assertions and its output is byte-identical
to `raku` v2026.07's: the five slice spellings, the single-element shapes that
must not move (a literal index, a `$` variable index, a `*-1` Whatever index,
and `.VAR.name` still naming the container), and the three default cases
(`is default`, a typed array, an untyped array/hash).

## Still open in the parent ticket

Native arrays (`@ints[0].VAR.^name` is `IntPosRef` in raku — mutsu has no
representation for a per-element-type positional ref) and nested subscripts
(`@sh[0;0]`, `%d<a><b>`), which turned out to be bigger than the parent ticket's
note said: raku answers `Scalar` / name `element` / default `(Any)` uniformly for
every nested element, and reaching that needs the *parent* container at runtime,
which a nested subscript has no name for. The parent ticket now records the
measurement and why it belongs with the descriptor rework.
