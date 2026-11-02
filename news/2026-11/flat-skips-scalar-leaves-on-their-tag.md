# `.flat` skips scalar leaves on their tag

`List.flat` (and `.flat.head(1)`) was O(n) before the first element: it scanned every element for an
`Iterable` instance (`flat_may_hold_iterable_instance`, #15322) and then, before dispatch,
`reify_or_consume_seq_target` walked the whole receiver recursively to reify nested deferred
`.map` / `.grep` Seqs (`reify_nested_map_grep_for_read`), cloning every element into a fresh `Vec`
and calling the recursive walker (stack check, `view()`) once per element -- ~157 of the 179 Ir per
element. Both passes now classify an element from its pointer tag alone: the scan with one new
probe (`flat_scan_class`: instance / not / list-or-slip), the walk with `is_scalar_leaf` (an `Int`,
`Num`, `Str`, `Bool`, `Nil` or type object holds nothing to reify), and the walk copies an array's
elements only when one of them is not a leaf. Same answers; a list with nested containers takes the
old path.

`(1..n).list.eager.flat.head(1)`, profiling build, warm: 14.1 ms -> 2.65 ms at n = 1,000,000 (rakudo
0.05 ms); callgrind, 179 -> 65 Ir per element, of which ~35 Ir is the benchmark building its list,
leaving ~30 Ir per element for `flat` itself. Still O(n) (the issue's close condition, O(1), is not met):
making it independent of n needs the instance check inside the lazy generator, which is a pure cursor
with no interpreter handle -- recorded on #15962.
