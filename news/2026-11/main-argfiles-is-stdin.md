# `$*ARGFILES` is `$*IN` inside `sub MAIN` (6.d)

From language version 6.d on, a read of `$*ARGFILES` inside `MAIN` (`lines`,
`get`, ...) reads standard input: the mainline has already consumed the files
named on the command line and `MAIN` receives them as arguments. `dispatch_main`
now swaps `$*ARGFILES` for `$*IN` for the duration of the call and restores it
afterwards; `use v6.c` keeps the old binding. Second of `roast/MISC/misc.t`'s
four failing subtests (#16412).
