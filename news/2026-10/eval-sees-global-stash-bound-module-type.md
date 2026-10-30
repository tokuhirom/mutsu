# A type published with `GLOBAL::<Name> := Name` resolves from a module's EVAL

`BEGIN GLOBAL::<User> := User` makes a module's type visible by its short name from every scope,
including a routine of another module that `EVAL`s a snippet naming it. The `EVAL` pre-check
(`Undeclared name`) and the runtime bareword lookup now consult GLOBAL's stash after the module
visibility gate, so `ORM::Factory`'s `find-definitions` (`EVALFILE` of a factory file that names the
test's models) works: `t/factory/configuration.rakutest` passes.
