# Regex capture names stay interned through matching

ADR-0016 left named capture axes keyed by owned `String`s after its P4 axis
collapse. A grammar parse therefore still allocated and compared capture-name
strings while applying candidates and recording undo entries.

`RegexCaptures` and stored `CapChildren` now key their named axes by `Symbol`.
The backtracking trail records the same compact symbol handle, and capture names
are resolved to strings only when a Raku-level `Match.hash` or a regex code-block
snapshot is materialized. This completes the first non-blocking residue recorded
by ADR-0016 without changing capture behavior.
