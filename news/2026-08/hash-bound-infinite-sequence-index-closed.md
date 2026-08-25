# Indexing an infinite sequence bound into a hash value works again

Binding a lazy infinite sequence into a hash value and then indexing far into it
through the hash used to divide by zero:

```raku
my @fib = 1, 1, * + * … ∞;
my %sequences;
%sequences<f> := @fib;
say %sequences<f>[100] / %sequences<f>[101];
```

The denominator read back as `0`, so the Fibonacci-ratio loop that surfaced this
died instead of converging on `0.6180339887498949`.

Re-running the ticket's full repro — both sequences, bound under two hash keys,
indexed through an iterated key across ten offsets — against current `main`
produces output identical to `raku`, so the underlying container-binding gap has
been closed by other work in the meantime. Unlike the sibling stale tickets
closed in the same sweep, this one had no regression test pinning it, so the
close-out adds `t/hash-bound-infinite-sequence-index.t`: it checks direct
indexing of the lazy arrays, indexing through the hash binding, the neighbouring-
element division that was the actual failure, and the iterated-key form from the
original report. The test passes unmodified under both `raku` and `mutsu`, so a
regression here now fails `make test` rather than silently reappearing.
