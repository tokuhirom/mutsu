# A hook module's `use` in a block of an EVAL binds in that block

A `use` of a module whose `sub EXPORT` hook returns the imports, nested in a
block of an `EVAL`'d string, used to be lifted out of the block and dropped, so
`MY::<&name>:exists` and `MY::.keys` inside the block missed the import
(#14480). The statement now stays in its block. The EVAL's CHECK-time pre-run
still runs the hook once and records the map it returned; the in-position `use`
replays that map instead of running the hook a second time.
