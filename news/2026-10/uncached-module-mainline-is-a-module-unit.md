# A module compiles as a module unit with the bytecode cache off too

A module's mainline used to fill its lexical import sets only when it was
compiled through the compiled-bytecode cache. With the cache off
(`MUTSU_PRECOMP_BYTECODE=0`, `--no-precomp`), or for a unit that may not use the
cache, the mainline was compiled as a plain block instead. Its top-level `use`s
then imported through env and registry aliases. The two paths gave the same
answers in the tests, but they were two import mechanisms for one construct.

Now a module's mainline is compiled the same way on both paths
(`compile_module_mainline_uncached`). Its top-level `use`s bind routines,
variables and types in the module's lexical import sets and pad, and they read
the canonical scans of the modules they `use` (ADR-8751, #14140).
