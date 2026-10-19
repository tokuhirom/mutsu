# A nested package with a same-named inner type is reachable by its short name

Rakudo publishes a nested package under its short name at the compunit's
top level when a type with the same short name is declared inside it. For
example, `module Enums { package Gender { enum Gender (...) } }` lets the
program write `Gender::Gender(1)`, and also `has Gender::Gender $.g`. mutsu
now does the same. The published name reaches a module's importer, and it
applies to calls, barewords, enum values and type constraints. Intl::CLDR's
`t/01-spot-check.rakutest` now passes all 11 subtests (#13940). Before, its
Units subtest died with "Could not find symbol '&Gender'".
