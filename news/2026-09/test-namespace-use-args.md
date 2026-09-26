# `use Test::Foo <args>` passes its arguments to `sub EXPORT`

The compiler routes every `use Test` / `use Test::*` statement through a
dedicated arm (the `Test` module is backed by mutsu's native provider), and
that arm emitted `UseModule` with no arguments. A real `Test::`-namespaced
compunit with a `sub EXPORT` therefore always saw an empty argument list:
`use Test::When <smoke>` never enabled its smoke-test gate, so the
distribution's own `t/01-env-vars.rakutest` failed all 8 subtests.

The `use`-argument evaluation now lives in one helper,
`Compiler::compile_use_export_args`, shared by the general `use` arm and the
`Test::*` arm. Test::When 2.1 moves from `partial` (1/2 files) to `green`
(2/2 files, 9/9 assertions). Pinned by
`t/modules/import-export/test-namespace-export-use-args.t`.
