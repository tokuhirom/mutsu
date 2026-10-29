# A file module's `our &name is export` no longer leaks into the loader

`collect_unit_our_var_names` lists the `our` variables whose bare env binding is
dropped after a unit's load (so only a real import brings them into scope), but
its first-character test skipped every `&name`. A file module's
`our &greet is export` therefore stayed bound as `&greet` in the loading scope,
even with `need`. `&` is now stripped like `@`/`%`, so the code variable stays in
its package (`&Module::greet`) until an import binds it.
