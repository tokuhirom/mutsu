# The core enums' constants are built on first lookup

Every scope sees the enums the core declares (`Order`, `Endian`,
`ProtocolFamily`, `Signal`, `SeekType`, `FileChangeEvent`, `PromiseStatus`)
by type name, by bare variant (`Less`, `SIGINT`) and by qualified variant
(`Order::Less`). `Interpreter::new` interned all ~120 of those names and
built a value for each into the process-wide base tier of the env: about 10%
of a `say "hello"`'s instructions, for constants most programs never name.

Now nothing is built at start-up:

- The enums live in one table, `builtin_enum_consts`. The runtime's variant
  lists (`.enums`, the registry's enum types, the type-constraint tables,
  `Promise.status`, `IO::Notification`'s events) derive from it instead of
  each keeping a copy. The signal table moves below the runtime layer with it.
- A name is recognized when it is interned. Its symbol flags mark it as a
  built-in enum constant and as a name an env lookup may find.
- Every env base tier answers such a symbol on a map miss, building the value
  on first lookup into a slot the whole process shares. The base tiers' key
  and entry walks include the constants, so symbolic lookup, `flatten` and
  the visible-name listings see them as before.

`say "hello"` drops from 3.44M to 3.08M instructions (callgrind, release,
`MUTSU_JIT=off`). Refs #19197.
