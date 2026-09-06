# `my @n = [\~] 1..*` no longer hangs

```raku
my @n = [\~] 1..*;
say @n[^5];        # raku: (1 12 123 1234 12345)   mutsu (before): hangs
```

Both halves worked in isolation — `([\~] 1..*)[^5]` was correct, and
`my @n = 1..*` preserved laziness — which is what made this look like a
triangle-reduce problem specific to `~`.

## What was actually wrong

The assignment forced the list, for **every** scan operator. `[\+] 1..*`
survived only because forcing 200_000 integers is fast; it reported
`.is-lazy` `False` (raku: `True`) and `.elems` `200000` (raku: throws
`X::Cannot::Lazy`), and took 235ms where it now takes 23ms. For `[\~]`, where
each element is one character longer than the last, the same forcing is
quadratic in bytes and never finished.

`LazyList::preserve_lazy_on_array_assign` — the predicate the `@`-assignment
consults — knew about an infinite `...` sequence spec, an endpoint-less closure
sequence and a `map`/`grep` pipe over an infinite source, but not about a scan.
`is_genuinely_lazy`, which answers `.is-lazy`, already carried an unconditional
`scan_spec` arm, so the two predicates simply disagreed: the value called itself
lazy right up to the moment the assignment reified it.

The arm needs no finiteness test the way the pipe case does. A scan becomes a
`LazyList` at all only when its input is already lazy (`exec_reduction_op`'s
`scan && input_is_lazy` gate), so a finite `[\+] 1..10` never reaches it and
still materializes into a real ten-element array.

Pinned by `t/triangle-reduce-stays-lazy-on-array-assign.t` — 14 assertions, all
measured against raku v2026.07 first: the doc example, `[\+]` and `[\*]` over
the same infinite range, `.head` and `[^n]` reads, and the finite scans
(`[\+] 1..10`, `[\~] <a b c>`) that must **not** become lazy.

## Filed, not fixed

Two neighbouring divergences turned up while sweeping; neither is caused by this
change (the expression form, which never goes through an array assignment, is
equally wrong):

- `todo/tickets/triangle-reduce-over-a-lazy-pipe-yields-nil.md` —
  `([\~] (1..*).map(* + 1))[^4]` is four `Nil`s. The same scan over a bare
  infinite range is correct for every operator, so it is the pipe-shaped
  *source* that the scan cannot pull from.
- `todo/tickets/reduce-with-a-bracketed-callable-op-does-not-parse.md` —
  `[\[&f]] 1, 2, 3` is a parse error. Reducing with a user-defined *infix*
  (`[\myop]`) already works in both forms, so the runtime can already fold with
  a user routine; the bracketed-Callable spelling is a syntax gap.
