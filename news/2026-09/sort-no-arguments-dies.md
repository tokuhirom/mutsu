# Bare `sort` with no arguments now dies

`sort;` and `sort()` — the sub form called with no positional argument at
all — silently returned an empty array. Rakudo has treated this as a
runtime error since the 2022.07 compiler release: `sort;` dies with
`Must specify something to sort` (an `X::AdHoc`).

`builtin_sort` (`runtime/builtins_collection_listops.rs`) used to fold two
different cases into one check: "no positional argument was given at all"
and "a positional argument was given but flattens to an empty item list"
(e.g. `sort(())`). Only the first is an error — the second genuinely was
told to sort an empty list, and legitimately answers `()`. The two are now
distinguished, and only the argument-free form dies.

Pinned by `t/collections/sort-no-args.t`, which also checks that
`sort(())` still returns `()` and that `sort()` as a feed-operator sink
(`(5, 1, 3) ==> sort()`) is unaffected — the feed lowering appends the fed
list as a positional argument before the call reaches this check.
