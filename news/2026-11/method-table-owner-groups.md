# The built-in method table finds an owner's rows without scanning every owner's

`owner_row`, `owner_rows_at` and `owner_declares` answered "which rows does
`Owner` declare for `method`" by scanning every row of that *name* and
string-comparing its owner. For a busy name the scan is long: infix `~` on an
`ObjAt` asks `role_declares(.., "Str")` across six role owners, each a scan of
every `Str` row (~1.8k instructions per probe), and an MRO walk for `new`
scans every `new` row once per class in the MRO.

Each name now carries its rows grouped by owner (sorted by the owner's symbol
id, each group in registration order), built on the name's first lookup, so a
lookup is a binary search plus the owner's own one or two rows. The revision
gate is still applied per lookup. A new oracle test compares both lookups
against a scan of every row.

`Monad::Maybe.some(1)` (whose `M.WHICH ~ '|' ~ T.WHICH` concatenation and
`.new` reach these lookups): 622k -> 592k instructions per call (callgrind,
profiling build, 200 calls, warm). Refs #17331.
