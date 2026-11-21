# `bless` runs an attribute's `set_build` closure

A metaclass can give an attribute its own initializer with
`Attribute.set_build(&code)`. `.new` ran that closure, but `.bless` ignored it
and gave the attribute its plain default instead, so a `%`-attribute meant to
start as a `SetHash` started as an empty `Hash` (#18463).

Red builds its model instances this way: `Red::Model.new` checks its
arguments and `nextwith`s into `Mu.new`, which blesses, and the
`%!___DIRTY_COLS_DATA___` attribute its metaclass adds is built as a
`SetHash`. As a `Hash` it stringified the attribute objects used as keys, and
marking an unknown id column dirty warned "Use of uninitialized value of type
Any in string context" (Red `t/023-metamodel-model`). `.bless` now runs the
closure (deferred past `BUILD` when the class has one), sharing the
override lookup with `.new`.
