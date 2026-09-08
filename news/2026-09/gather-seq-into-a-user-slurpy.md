# A `gather` / sequence-operator Seq arrived as one element in a user slurpy

```raku
sub f(*@a) { say @a.elems }; f(gather { take $_ for 1..4 });  # mutsu 1, raku 4
sub f(*@a) { say @a.elems }; f((1, *+1 ... 4));               # mutsu 1, raku 4
sub f(+@a) { say @a.elems }; f(gather { take $_ for 1..4 });  # mutsu 1, raku 4
```

This is the surviving half of the "a lazy Seq vanishes into a user slurpy"
report. The `.map` and `.grep` halves were fixed earlier, when the `*@a` binder
learned to reify a deferred map/grep `Seq` (the `*@` twin of ADR-0058 step 3b's
`+@` reification). `gather` and the sequence operator kept failing, and with a
**different mechanism**: the slurpy was not empty, it held the sequence itself
as a single element (`@a[0].^name` was `Seq`, `@a[0].elems` was 4).

## Root cause

The binder's flatten arm matches on `arg.view()`, and its iterable arms name
`Array`, the four `Range` shapes, `GenericRange`, `Seq` and `Slip`. A `gather`
block and a sequence-operator sequence are neither: they are `ValueView::
LazyList`. So they fell through to the catch-all `_ => items.push(arg)` and
became one element.

The single-argument `single_lazy_value` branch above the loop *does* look at
`LazyList`, but it only fires for a **genuinely lazy** one — deliberately, since
that is what keeps `f(1..Inf)` from hanging. A finite `gather` reports
`is_genuinely_lazy()` false (its `closure_seq` has an endpoint), so it fell past
that branch as well, and then past every iterable arm.

`flatten_into_slurpy` itself could not have covered this: forcing a `gather`
means running user code, and it is a pure function with no `Interpreter`.

## The fix

Both slurpy binders now force a `LazyList` argument that reports itself finite,
and flatten the result:

- the `*@a` flatten loop, for every positional argument;
- the `+@a` single-argument rule, whose `_ => vec![single]` arm boxed the same
  values (`sub f(+@a) { @a.elems }; f(gather {...})` was also `1`).

Forcing goes through `Interpreter::force_lazy_list_vm`, so a gather body runs
under the VM the way every other consumer runs it. A genuinely lazy source is
left alone — the guard is `!ll.is_genuinely_lazy()` — so `f(1..Inf)` still binds
lazily through the `single_lazy_value` branch and `f((1..Inf).map(*+1))` is
still reify-on-index rather than a hang.

## Pin

`t/lazy-seq-into-user-slurpy.t` grows from 12 to 31 assertions. Its `todo` on
the gather row is gone, and it now covers all four producers (`.map`, `.grep`,
`gather`, the sequence operator) into a `*@a` slurpy, a `+@a` slurpy and a
non-slurpy `@a`, alone and alongside other arguments — the three paths used to
disagree with each other. The laziness controls (`.is-lazy` on an infinite
Range and an infinite sequence, indexing into an infinite `.map` pipe) are the
guard rail: forcing any of them would hang. The whole file also passes
unmodified under real Rakudo.

Closes [#7591](https://github.com/tokuhirom/mutsu/issues/7591).
