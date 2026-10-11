# `callsame` from a user `map`/`grep`/... reaches the built-in row

A user method that overrides one of `Any`'s list methods and defers with `callsame` or `nextsame`
now has a test pinning that the deferral reaches the built-in method-table row: `map`, `grep`,
`first`, `reduce`, `produce`, `rotor`, `skip`, `squish`, `classify`, `categorize`, `match` and
`eager` on an `is Array` subclass answer exactly what Rakudo answers (ADR-11276 §9.77). That was
the last open item of the `Any` interpreter rows (#12388).

The ADR also records what a call through such a row costs: about 590 instructions of wrapper
around the 1,640 of `dispatch_map_method` itself, out of roughly 35,000 for a whole
`$l.map(* + 1)` call on a three-element list.
