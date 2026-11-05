# Atomics and worker stores address a lexical's place, not its name

ADR-16595 slice 3. Once any thread had run, `cas(@d[0], ...)` in a block whose
`my @d` shadows an outer `@d` swapped the outer array's element, a worker's
`push` onto its own `my @shadowed` landed in the parent's `@shadowed`, and
sibling workers' `my @w` merged, because those operations located the container
in the name-keyed cross-thread lane.

A plain user `@`/`%` lexical now never uses the lane (`container_lane_applies`):
element `cas`, `⚛` and `atomic-fetch-*` promote the element of the container the
frame (or the captured cell) holds, under that container's ADR-0068 lock, and
push, element and whole-container reads and writes go to the slot or cell. The
lane keeps attributes, dynamics, internal `__mutsu_*` keys and the opaque
names a registered class/role method writes. With `container_name_is_redeclared`
forced to `false`, the three files that failed on it and the four extra files
of the ADR's configuration A pass, so the masks are no longer load-bearing for
these routes (slice 4 deletes them).
