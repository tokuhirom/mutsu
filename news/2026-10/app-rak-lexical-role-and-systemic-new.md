# App::Rak loads: per-module `my role` scope and `Compiler.new` / `VM.new`

`App::Rak`'s only test file now passes. Two interpreter gaps blocked it:

- A module body is its own lexical scope for `my role`. A second module declaring a
  `my role Type` used to continue the first module's role (and was attributed as the
  provider of the bare name), so its routines died with `Undeclared name: Type`.
  Module loads now isolate the pending lexical-declaration records, and a `my role` no
  longer publishes its bare name to the module merge, as for `my class`.
- `Compiler.new`, `VM.new`, `Distro.new` and `Kernel.new` return the running process's
  instance, as Rakudo does, instead of an attribute-less object whose `.name` was `Nil`
  (`META::constants` builds its credits string from them).
