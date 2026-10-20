# A `sub EXPORT` hook's routines bind only in the importing block

A plain routine a module's `sub EXPORT` hook returns (`'&hf' => sub { ... }`)
was bound in the block's lexical import set and was also written into env as
an alias that the block's import scope rolled back. The alias is gone now
(ADR-8751, #14140). `install_export_symbol` hands such a routine to
`bind_hook_routine_lexically`, which keeps it in the running import capture,
and the set binds it from there. Every call site in the block already read
the set. An operator or a term keeps the env alias, because its built-in op
or bareword still reads it.

The BEGIN-time preload of a module that only a nested block of the program
`use`s now installs none of the hook's exports, routines included.
`if False { use M }; EVAL 'hf()'` used to find the hook's routine for the
rest of the program.
