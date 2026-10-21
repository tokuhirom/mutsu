# `require` below the top level binds its imports lexically

`require M <&f $x T K>` inside a block, routine or closure body now binds the
listed symbols in that scope, as a block-scoped `use` does (ADR-8751, #14140).
The call stores each listed symbol's value in a hook-value set, and the scope
goes on with `my &f := ...; my $x := ...; my \T := ...` bound from it
(`OpCode::GetHookExport`). A closure taken in the block keeps working after the
block's import scope ends, and a routine declared outside the block no longer
sees the required routine: it was installed as a `GLOBAL::` registry name that
every routine called from the block could resolve.

`require M` also stopped importing the module's exports. It now loads the
module the way `use`'s BEGIN-time preload does and imports only the symbols it
lists, reading a listed variable or constant from the module's export table
(Test, whose routines the load itself provides, still imports on load). Before,
`require M; f()` called an exported `f` that Rakudo rejects as undeclared.
