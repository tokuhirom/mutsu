# A `.map` pipe over `IO::CatHandle.handles` never forces, so `.raku`/`eqv` see `(...)`

`$cat.handles.map({ ... })` produces a lazy pipe (`LazyList` with
`lazy_pipe`) whose source is the cat's `cat_pull` list. Asking that pipe for
`.raku` answers the opaque `(...)` placeholder instead of its elements, and
`eqv` against a Seq answers False without running the callbacks. Every other
finite lazy pipe forces correctly.

## Symptom

`t/io-cathandle-lazy.t` test 10 (`lazy .handles: reads 2 lines per handle`)
fails under `MUTSU_REAL_TEST=1`:

```
# expected: $(("a1", "a2"), ("b1", "b2"), ("c1", "c2"))
#      got: (...)
```

It passes under mutsu's native `Test` provider only because that provider's
`is-deeply` compares differently; the underlying value is wrong either way.

## Repro (no Test module involved)

```raku
sub tmpfile($content) { my $p = "tmp/cat-{$*PID}-{$++}.txt".IO; $p.spurt($content); $p }
my $cat := IO::CatHandle.new: tmpfile("a1\na2\na3\na4"), tmpfile("b1\nb2\nb3\nb4");
my $m = $cat.handles.map({ eager .lines: 2 });
say $m.^name;   # Seq        (both)
say $m.raku;    # mutsu: (...)   raku: renders the elements
```

## What is known

`Self::lazy_list_needs_forcing("raku")` is true and `"raku"` is not in
`lazy_pipe_preserving_coercion`, so the forcing branch in
`src/vm/vm_call_method_mut_ops.rs` (and its twin in `vm_call_method_ops.rs`)
*should* run — yet neither the force nor the `X::Cannot::Lazy` rejection
happens, which points at `needs_vm_lazy_dispatch()` returning false for this
shape and the call falling through to a native `.raku` on an unfilled body.
`pipe_bottoms_out_finite()` also needs checking for a `cat_pull`-rooted chain:
the 2026-08-28 `eqv` fix (`LazyList::eqv_would_hang` now excludes a
provably-finite pipe) did not change this file's behaviour, so the cat-rooted
pipe is probably not recognised as finite either.

Both questions are in the same small area and should be answered together.
