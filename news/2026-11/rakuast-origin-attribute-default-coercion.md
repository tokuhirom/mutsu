# RakuAST origins: attribute defaults and coercion types

An attribute's default (`has Str:D $.s = "x"`) had no `.origin` on its
`Initializer::Assign` or its value: the default is converted twice, for the
hidden `Trait::WillBuild` and for the initializer, and the trait, converted
first, took the written term's range. The initializer is now converted first,
and the trait shares its node for a literal default, as rakudo's does.

A coercion type (`Str(Int)`, `Str()`) of a declaration or a parameter is
placed as rakudo 2026.11 places it: the `Type::Coercion` and its base
`Type::Simple` over the whole coercion, the base's `Name` over the name before
the `(`, and the constraint over the text between the parentheses. A
parameter's declaration visits the coercion type too (#15780).
