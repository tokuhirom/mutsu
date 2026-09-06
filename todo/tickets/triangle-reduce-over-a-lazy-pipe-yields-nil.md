# `[\~] (1..*).map(...)` yields `Nil` for every element

Found 2026-09-06 while fixing
`todo/tickets/array-assignment-eagerly-reifies-a-triangle-reduce.md`
(`news/2026-09/triangle-reduce-stays-lazy-through-an-array-assignment.md`),
whose neighbourhood list asked whether a scan over a `.map` pipe behaves like
one over a bare range. It does not. Pre-existing and independent of that fix:
the *expression* form, which never goes through an array assignment, is equally
wrong.

## Repro

```raku
say ([\~] (1..*).map(* + 1))[^4];
# raku:  (2 23 234 2345)
# mutsu: (Nil Nil Nil Nil)

say ([\~] (1..*).map(* + 1)).head(4);
# raku:  (2 23 234 2345)
# mutsu: ()
```

## What already works, and why that narrows it

The same scan over a bare infinite range is correct, for every operator tried:

| Program | raku | mutsu |
|---|---|---|
| `([\~] 1..*)[^5]` | `(1 12 123 1234 12345)` | same — correct |
| `([\+] 1..*)[^5]` | `(1 3 6 10 15)` | same — correct |
| `([\*] 1..*)[^5]` | `(1 2 6 24 120)` | same — correct |
| `[\~] <a b c>` (finite) | `[a ab abc]` | same — correct |
| **`([\~] (1..*).map(* + 1))[^4]`** | `(2 23 234 2345)` | **`(Nil Nil Nil Nil)`** |

So the operator and the scan protocol are fine; only the *source* being a lazy
`.map`/`.grep` pipe rather than a range breaks it. And it is not merely
producing too few elements — `[^4]` yields four `Nil`s, so the scan is stepping
but reading nothing out of the pipe.

## Where to look

`Interpreter::exec_lazy_scan_reduction` (`src/vm/vm_misc_reduction_scan.rs`)
builds a `crate::value::ScanSpec { source, .. }` and hands it to
`LazyList::new_scan`; the pulling happens in `force_scan_lazy_list`. Find how
that pull reads the next element of `spec.source` and why a `lazy_pipe`-backed
`LazyList` source yields nothing where a `Range` source yields its elements —
the `sequence_spec` / `lazy_pipe` / `closure_seq` shapes each reify differently
(see `LazyList::preserve_lazy_on_array_assign` and `pipe_bottoms_out_finite`
for the taxonomy).

## Neighbourhood to check when fixing

`.grep` as well as `.map`; a pipe over a *finite* source
(`[\~] (1..10).map(* + 1)`, which must materialize); a `gather`-backed source; a
scan over a `...` sequence spec (`[\+] (1, 2, 4 ... *)`); the array-assigned
spelling `my @e = [\~] (1..*).map(...)`, which must stay lazy (that is what the
fix above pinned); and `.head`, `[^n]` and a `for` loop over the result, since
`.head(4)` and `[^4]` fail differently today (empty list vs four `Nil`s).
