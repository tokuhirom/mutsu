# Element atomics on an `@`/`%` declared after the first `start` land in the container

`await start { 1 }; my @d = 0, 0, 0; cas(@d[1], 0, 5); say @d` printed
`[0 0 0]`: every element atomic (`cas(@a[i], ...)`, `cas(%h{k}, &code)`, the
multi-dimensional `cas` forms, `atomic-fetch-inc(@a[i])`, `@a[i]⚛++`, ...)
wrote to the name-keyed `__mutsu_atomic_arr::@d` / `__mutsu_atomic_hash::%h`
lane, while `GetLocal` deliberately skips that lane for a name this lineage
re-declared (or an aggregate parameter) and reads the frame's own slot. The two
sides disagreed on which store owned `@d`, so the write was lost — and since the
lane is keyed by bare name, an inner `my @d` could even swap an element of an
unrelated outer `@d`.

The compiler now passes the container's own read alongside its name, and
`Interpreter::atomic_elem_cell` resolves the element for every element atomic.
A shared container keeps the lane (every thread meets the same cells under its
write lock); a re-declared one has its element promoted in place to a
`ContainerRef` cell inside the binding the frame holds — the same promotion
`my $r := @a[1]` performs — so the reader sees it. Spawning a thread clears the
re-declaration mask and publishes the binding, so a container that later becomes
shared moves back to the lane with its cells intact (#11833).
