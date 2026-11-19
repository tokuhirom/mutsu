# A module's file-scope lexicals are shared by every thread

A module's file-scope `my` variables live in the compunit statics store
(`unit_lexicals`). A spawn used to fork that store. So a worker started before
a module was loaded had no copy of the module's statics, and the module's
routines running on that worker read them as `Any`. They could also read them
as the caller's same-named variable: Test's `ok` on Green's worker took
Green's `my Str $output` for its own `$output` and died with
`Cannot resolve caller say(Str:D: Str:D)`.

The store and its two companion tables are now world tables shared by every
thread (ADR-17216 slice 4c, #18136), like the package statics of slice 4a. A
write on one thread is seen on every other. In-place mutations go through a
per-thread pinned root, so no reference into the table outlives its lock.
