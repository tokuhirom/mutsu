# A `:=`-bound array's element store now takes the fast lane

#8107 and #8151 took the single-threaded `@a[$i] = $v` from ~1,940 ns to ~277 ns by adding
`try_fast_array_element_assign` and then consulting it before the store's shared preamble. That lane
serves a **plain** array. It declined outright for a `:=`-bound one, and nothing had measured what
the decline cost.

It cost 4.9x. Two programs identical apart from the `:=`, 400,000 stores into a 64-element array at
mainline scope:

| per store (incl. the loop) | mutsu | raku |
| --- | ---: | ---: |
| plain `@arr` | 782 ns | 224 ns |
| `:=`-bound `@z` | **3,823 ns** | 226 ns |

rakudo does not care which one it is. mutsu paid ~3,000 ns for the binding.

## One guard, not the three that looked likely

Instrumenting each decline point settled it, and two of the three suspects turned out to be
innocent:

- `try_fast_array_element_assign_early`'s `unit_lexical_container_cell` guard — the one whose
  ADR-0039 seed/restore reasoning makes it the delicate part — **does not fire** for this shape. It
  opens with `unit_lexicals.is_empty()`, and a program with no mainline named sub capturing a
  file-scope `my` has none. It keeps declining for the captured case, which is the conservative
  answer anyway.
- `is_readonly_sym` **does not fire** either, which is worth recording because a `:=`-bound
  container is documented as landing in `readonly_vars` alongside genuinely readonly names (that is
  why `Stmt::MarkBoundContainer` exists). Whatever the reason, the guard measured as inert for this
  shape and needed no change; a store that does reach it still declines.

What declined was one line — the lane resolves its target as

```rust
let (items, kind) = self.env().get_sym(var_sym).and_then(|v| match v.view() {
    ValueView::Array(items, kind) => Some((items.clone(), kind)),
    _ => None,
})?;
```

and a bound container is a `ContainerRef` wrapping a `ContainerCell`, so the `and_then` yielded
`None` before the lane looked at anything else.

## Descending one cell

The lane now accepts a `ContainerRef` and reads the `Array` out of its cell. That is sound here for
a specific reason: the commit at the bottom of the lane mutates the backing node **in place**
through `gc_contents_mut`, so the cell's inner `Value` keeps pointing at the node it already pointed
at and every alias sharing the cell observes the store. The cell is never replaced, so nothing has
to hold its lock past reading the handle out. A holder-local itemization flavour
(`container_ref_is_itemized`) still declines, because that changes what a read of the cell yields
and this lane does not reproduce that rule.

The dual-store coherence check had to be restated rather than reused. For a plain array it asks
whether the local slot holds the same backing node; for a bound one the slot holds the **cell**, and
the cell is the one thing every alias genuinely shares, so cell identity is what makes the in-place
write visible to both halves. A slot holding a bare `Array` while env holds a cell — or the reverse
— is exactly the divergence the check exists to refuse, and now does.

`multi_mutator_threads_live()` keeps its decline untouched; the concurrent half is #8069 §4.2-§4.4.

## Result

Measured within one binary on each side, so the latch state is identical for both rows of a column
(each program declares an array `:=`; only the store's target differs):

| 400,000 stores | before | after |
| --- | ---: | ---: |
| into a plain array | 892 ns/store | 898 ns/store |
| into the `:=`-bound array | **3,400 ns/store** | **935 ns/store** |
| bound / plain | 3.8x | **1.04x** |

The bound container is now exactly as cheap as a plain one. `benchmarks/bench-threads-serial.raku`
and `benchmarks/bench-threads.raku` both write through a `:=`-bound `@slots` deliberately — their
headers say the bindings are load-bearing and must not be simplified away — so this is ~80,000
stores' worth of the row that reads as "threads are slow". The other two thirds of that row are
[#8306](https://github.com/tokuhirom/mutsu/pull/8306) (the `:=` latch tax, landed separately) and
[#8308](https://github.com/tokuhirom/mutsu/issues/8308) (the indexed element *read*, still ~330 ns
of marginal cost against rakudo's ~39 ns).

Pinned by `t/collections/celled-array-element-store.t`: writes visible through every alias, a
by-value copy that detaches, element and container type constraints, autovivification past the end,
a shaped array, an element that is itself a container, an immutable `List`, the array stored into
its own element, and a three-name bind group — all 14 cases green under rakudo too.
