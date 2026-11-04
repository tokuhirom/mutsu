# `has_type_capture_binding` answers no for a name no `::T` marker was ever written for

Every typed parameter binding asks `has_type_capture_binding(constraint)` (several times: the
constraint, its base, ...). Once any `::T` capture has been bound in the process (loading
NativeCall is enough), each ask interned the name and probed the env for the marker key. The two
places that write a marker (`bind_type_capture`, `type_capture_marker`) now also set a bit in a
256-bit process-global filter keyed by the name; a name whose bit is clear has no marker anywhere
and is answered without the intern and the probe. A set bit falls through to the exact check, so a
collision costs only the old work.

Callgrind slope on a profiling build, second run: fill 61.4k -> 60.2k Ir per store, fill +
`.Array` 148.7k -> 147.1k Ir per element (-1%). Refs #16215.
