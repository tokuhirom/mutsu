# A `start` block captures its celled `@`/`%` lexicals by cell, not by name

ADR-12204 slice S2, first step. `block_captured_scalars` now also names the
`@`/`%` lexicals a spawned block references that already live in a shared
`ContainerRef` cell in the spawning frame (and are not written by a registered
class/role method, nor attributes, dynamics or compiler temporaries). Those
names get no name-keyed lane entry: parent and child hold one cell, and a
second binding of the same name is a different cell. Names the analysis cannot
see stay on the lane. `MUTSU_SPAWN_CAPTURE_CHECK=all` prints the cell-captured
set and reports a `cell-captured-lane-write` if one still reaches the lane.
Block-less spawns (`tap`/`whenever`/`.then`, readers) are the next step.
