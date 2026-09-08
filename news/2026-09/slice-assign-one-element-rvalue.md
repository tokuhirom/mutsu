# A one-element slice assignment yields a one-element list again

A slice assignment's value is the list it stored, however short. mutsu
collapsed a one-slot slice to its element:

```
$ ./target/debug/mutsu -e 'my @d; say (@d[0,] = 5).raku'   # was: 5
$ raku               -e 'my @d; say (@d[0,] = 5).raku'     # (5,)
```

`.elems` agreed on both (1), and the *storage* was always right — only the
shape of the assignment's own value was wrong. The controls are what made this
narrow: a genuine single-index assignment yields the bare value on both
(`(@d[0] = 5)` is `5`, `(%h<a> = 5)` is `5`). A one-element **slice**
(`@d[0,]`, with the trailing comma) names a list of one slot, and that is the
shape that diverged.

## Root cause

`idx_is_single_element` (`src/vm/vm_var_assign_index_named.rs`) was doing two
different jobs. Its real job is answering "does this subscript name exactly one
key" — which is what keeps an *itemized* index a single key rather than a slice
(`my $s = $(1,2); %c{$s}` is one key, not two), and that job is load-bearing.
It was also being reused to decide the **rvalue's shape**, and the two questions
have different answers for a one-element list subscript: it names one key, but
it is still a slice.

## The fix

A companion `idx_is_scalar_subscript`, computed from the same raw index, answers
only the rvalue question: a non-itemized `Array`/`Seq`/`Slip` subscript is a
list however short, everything else is a scalar index. `idx_is_single_element`
is untouched, so the itemized-key logic keeps the answer it needs, and the
slice arm's result now itemizes only when *both* hold.

The slice's own arity decides the rvalue's length, not the RHS's, which falls
out of using the values actually stored: `(@d[0,] = 5, 6)` is `(5,)` and `@d` is
`[5]`, matching rakudo.

`t/slice-assign-one-element-rvalue.t` pins it — including every control the
ticket named — and passes under rakudo as well as mutsu.

## Not fixed here

The **Range** spelling of the same shape (`@d[0..0] = 5`) still yields a bare
`5`. It has a different root cause: a Range subscript on a plain positional
array is never expanded into an index list, so it never reaches the slice arm
that builds the stored-value list, and the multi-element case only looks right
because returning the raw RHS happens to match. Filed as
[#7651](https://github.com/tokuhirom/mutsu/issues/7651) rather than widening
this change.

Closes [#7589](https://github.com/tokuhirom/mutsu/issues/7589).
