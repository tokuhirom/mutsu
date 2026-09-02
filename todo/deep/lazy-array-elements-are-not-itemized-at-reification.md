# A lazy source assigned to `@` reifies BARE elements, not itemized ones

ADR-0040 makes a real `Array`/`Hash` itemize every element it stores, so an
element that holds a list reads back as `$(2, 3)`. That holds for every eager
store, but NOT for a lazy source assigned to an `@` variable:

```raku
my @lza = lazy gather { take $_ for 1, (2, 3) };
say @lza[1].raku;   # raku: $(2, 3)   mutsu: (2, 3)
```

Measured 2026-09-02 on `main` + ADR-0064.

## Root cause

The assignment stores the `LazyList` itself; nothing is reified yet, so the
store-side itemization hooks (ADR-0040 slices 1-2) see one lazy value, not a
vector of elements. The elements first exist when the subscript forces the
list, in `Interpreter::resolve_index_value` (`src/vm/vm_var_index_ops.rs`,
the `ValueView::LazyList(ll)` arm ending in `target = Value::array(forced)`).

That site cannot itemize on its own, and this is the hard part: it is
**name-blind**. A `LazyList` is the reified form of BOTH a real `Array`
assigned a lazy source (elements ARE containers) and a lazy `Seq` (elements
are the values). ADR-0040 slice 3 resolves exactly that ambiguity from the
variable's sigil (`container_elements_are_containers`'s `LazyList` arm keys
off `source_name.starts_with('@')`), and the sigil is not available in the
value-level index path.

So the fix is a representation question, not a patch: either the `LazyList`
has to carry "I am a real Array's backing" from the assignment that created
it, or the `@`-assign has to wrap the lazy source in something that itemizes
as it pulls.

## Why it matters beyond `.raku`

ADR-0064's slice discriminator (`builtin_index_var_meta`,
`src/runtime/builtins.rs`) tells a slice from an element by asking whether the
value is a bare, non-itemized `List` -- sound precisely because a real
container itemizes its stores. It has to carve out `LazyList`-backed arrays
for that reason, and pays for it: a SLICE of one reports `Scalar` instead of
`List`.

```raku
my @lza = lazy gather { take $_ for 1, (2, 3) };
say @lza[0,1].VAR.^name;   # raku: List   mutsu: Scalar
```

Closing this hole removes that carve-out and the residual with it.

## Repro

```
raku  -e 'my @l = lazy gather { take $_ for 1, (2,3) }; say @l[1].raku'   # $(2, 3)
mutsu -e 'my @l = lazy gather { take $_ for 1, (2,3) }; say @l[1].raku'   # (2, 3)
```

Pinned (as the current behaviour) by `t/element-store-itemization.t`
test 139, which asserts the reflection side is already right
(`@lza[1].VAR.^name` is `Scalar`) even though the value side is not.
