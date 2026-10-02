# `constant`/`enum` written before `unit class` stay visible to the importer

A module file's `constant` or `enum` declared before its `unit class Foo;` / `unit module Foo;`
line lives in the compunit mainline (GLOBAL), so rakudo leaves it visible to the importing scope.
mutsu dropped every file-scope constant of a `unit` compunit from the importer, which broke
Dist::META's `t/00-sanity.t` (`%phases-eq<build>` read from the test). Only declarations after the
`unit` statement are now treated as package-private. Pinned by
`t/vm/binding/constant-before-unit-visible-to-importer.t`.
