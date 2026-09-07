# `[\op]` over a lazy pipe answers, and stays lazy

```raku
say ([\~] (1..*).map(* + 1))[^4];
# raku:  (2 23 234 2345)
# mutsu: (Nil Nil Nil Nil)

say ([\~] (1..*).map(* + 1)).head(4);
# raku:  (2 23 234 2345)
# mutsu: ()
```

The same scan over a bare infinite range was already correct for every operator
tried, and so was a finite list — only a `.map`/`.grep` *pipe* as the source
broke, and it broke in two different ways depending on how the result was read.

## Root cause

`force_scan_lazy_list` special-cased every `Range` shape and fell through to
`value_to_list` for everything else. `value_to_list` declines to materialise an
unbounded `LazyList`, so it answered an EMPTY list and the scan stepped
`needed` times over nothing — producing that many `Nil`s. The walk now pulls
exactly the prefix each batch needs (`force_lazy_list_vm_n`), which is the same
bounded pull every other lazy consumer uses; a finite pipe simply runs out and
the scan ends with it.

## …and the eager pre-compute had to go with it

The construction (`exec_lazy_scan_reduction`) pre-computed a 1000-element batch
so that eager consumers reading the element cache directly got a useful prefix.
Over a `Range` that is a thousand cheap reduction steps; over a *pipe* it runs
the user's closure a thousand times, which is observable:

```raku
say ([\+] (1..*).map({ die "too far" if $_ > 5; $_ }))[^3];   # must be [1 3 6]
```

So the pre-compute is skipped for a lazy source, and the consumers that were
relying on it now pull properly instead. Two places needed teaching:

- `force_lazy_list_vm_n_inner` (the *bounded* pull, which `.head(n)` uses) had
  no `scan_spec` arm — only the unbounded `force_lazy_list_vm_inner` did — so it
  read the empty element cache and answered `()`.
- `LazyList::needs_vm_lazy_dispatch` did not name `scan_spec`, so a scan list
  never entered the VM's lazy dispatch block at all. The 1000-element
  pre-compute had been masking that.

## Scope

Pinned by `t/triangle-reduce-over-a-lazy-pipe.t` (15 assertions measured against
rakudo 2026.07): the repro by subscript and by `.head`, a `.grep` pipe, the
already-correct shapes as invariants (bare infinite range for `~`/`+`/`*`, a
finite list, a `...` sequence spec), a finite pipe that must materialize, the
array-assigned spelling that must stay lazy (the pin from
`news/2026-09/triangle-reduce-stays-lazy-through-an-array-assignment.md`),
`.is-lazy`, and three bounded-pull rows including the `die`-past-five case
above. The whole file also passes under `raku` unchanged.
