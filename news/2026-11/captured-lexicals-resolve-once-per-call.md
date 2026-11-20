# A sub resolves a captured outer lexical once per call

A named sub declared at a program's top level or in a bare block that reads a
lexical of that scope (ADR-0024) walked the whole by-name resolution chain on
every read: the stores that might answer first, the routine's capture bucket,
the cell. A loop body reading such a variable paid about 2.9k instructions per
read on top of what a local read costs.

The per-call cache routine-nested `my sub`s already had (#17971) now serves
these subs too. The second read of a name in a call records the binding the
chain answered with, and later reads load it from the cell. A write by the sub,
by a callee or by a recursive activation still shows, because they all write
through the same cell.

The cache also changed shape. It used to hold one frame's entries and clear
them whenever another frame filled, so a nested sub calling a sibling lost its
cached reads. It now keeps a caller's entries across the calls it makes, and
drops only the entries of frames that returned. Each entry records its frame's
routine-stack index, so a returned frame is recognized in one comparison.

On a profiling build (callgrind slope), a read of a unit-level `my` inside a
top-level sub's loop fell from 5.4k to 2.8k instructions per iteration; a local
read is 2.5k. A routine-nested `my sub` called in a loop fell 7%. A sub that
reads its captured variable once per call pays about 1% more, for the
placeholder its first read leaves. This is ADR-18244 stage B, slice 2 (#18274).
