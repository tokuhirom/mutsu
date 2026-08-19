# An element assign into a lazy array reifies 100,000 elements and drops laziness

Writing a single element of a lazy `@`-array materializes a hard-coded 100,000-element
prefix and converts the value to a plain `real_array`, so the array stops being lazy.
raku reifies only up to the touched index and keeps the array lazy.

```
$ mutsu -e 'my @d = 1, 2, 3 ... Inf; @d[2] = 99; say @d.elems'
100000
$ raku  -e 'my @d = 1, 2, 3 ... Inf; @d[2] = 99; say @d.elems'
Cannot .elems a lazy list
```

Two defects, one cause: the reified prefix is not bounded by what was touched, and the
laziness marker is discarded along with the live tail.

## Cost

With a geometric sequence every one of those 100,000 elements is a `2**n` bigint, so a
three-element write allocates hundreds of megabytes:

```
$ mutsu -e 'my @d = 1, 2, 4 ... Inf; @d[2] = 99; say "ok"'     # 1.18 s, 666 MB peak RSS
$ mutsu -e 'my @d = 1, 2, 3 ... Inf; @d[2] = 99; say @d.elems' # 0.03 s,  47 MB peak RSS
$ raku  -e 'my @d = 1, 2, 4 ... Inf; @d[2] = 99; say @d[^4]'   # 166 MB peak RSS
```

Found while timing the whole `t/` suite: `t/lazy-array-assign-preserve.t` was the single
memory outlier at 670 MB peak, against a ~50 MB baseline for every other file (the next
highest non-panic test is 117 MB). Bisecting its blocks pinned the cost to the one line
`my @a = 1, 2, 4 ... Inf; @a[2] = 99;`.

## Where

`Interpreter::reify_lazy_array_slot` in `src/vm/vm_helpers_lazy.rs` (the `MAX_ARRAY_EXPAND
= 100_000` inside it). Callers:

- `src/vm/vm_var_assign_element.rs:418` -- element assign (`@a[i] = v`)
- `src/vm/vm_var_delete_ops.rs:199` -- `:delete`

The behaviour is deliberate and documented as an approximation: the function's own doc
comment says "Front mutations collapse the list to its prefix (no worse than the pre-L2
capped Array)", and `docs/lazy-arrays.md` lists `100_000` among "the capping points". So
this is a known shortcut whose cost was never measured, not an oversight.

## Why it is deep, not a ticket

The cheap-looking fix -- reify only up to the touched index -- is not sufficient on its
own, because the value that comes back must still *be* lazy: the tail has to stay live so
that `@d.elems` keeps throwing `X::Cannot::Lazy` and a later `@d[10]` still reifies from
the source. That means a mutated lazy array needs a representation with a mutable reified
prefix over a live source, rather than today's "reify to a cap, then hand back a
`real_array`". `LazyList` already carries a `cache`, so writing the override into the
cache and returning the `LazyList` may well be the shape of the answer, but the semantics
of every consumer that currently relies on getting a real Array back after a mutation
have to be checked first -- including the `:delete` path, which has to express a hole in
a lazy list.

`docs/lazy-arrays.md` should be updated in the same change: its table and its "capping
points" section describe the current approximation as the design.

## Test that hides it today

`t/lazy-array-assign-preserve.t` asserts

```raku
my @a = 1, 2, 4 ... Inf;
@a[2] = 99;
is-deeply @a[^4], (1, 2, 99, 8), 'element assign reifies a prefix, tail stays live';
```

which passes, because it only inspects the first four elements. The description claims
"tail stays live" while the tail has in fact been flattened into a finite 100,000-element
array. When this is fixed, extend that block to assert the array is still lazy
(`@a.is-lazy`, or that `.elems` throws `X::Cannot::Lazy`) so the description becomes true.
