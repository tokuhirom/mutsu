# A named sub inside an `if` branch is visible to the whole branch

`if COND { do_tests(...); sub do_tests(...) { ... } }` died with "Unknown function: do_tests": a
`sub` declared in an `if`/`else`/`unless` branch was registered where it appears, not at the top of
the branch, so a call above it found nothing (a bare block, a `for` body and a routine body already
hoisted). Both the statement-position and the value-position branch compilers now hoist the branch's
named subs. Linux::Fuser's `t/01-basic.t` relies on it.
