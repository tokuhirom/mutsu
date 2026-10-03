# `use Outer::Inner` makes the bare name `Outer` visible

`Monad::Maybe` begins with `use Monad;` and declares `role Monad::Maybe is
Monad`. A test that only `use`s `Monad::Maybe` then asks `$x ~~ Monad`.
Rakudo resolves that: merging the module's GLOBAL brings in the package
`Monad` that `Monad::Maybe` is nested under, and in that module the package is
the class it `use`d. mutsu's module-visibility gate (ADR-11136) already let the
qualified `Monad::Maybe` through on that package grant, but hid the bare
`Monad` because its provider module was never merged directly. The bare-name
gate now accepts the same grant. A module that merely `use`s a class without
nesting under it still does not re-export it. Monad's seven test files all
pass.
