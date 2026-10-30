# `Str.AST` renders a caller-declared parent as a `Type::Simple`

`class Base2 { }; Q[class F is Base2 { }].AST` gave `Trait::Is(name => ...)`; rakudo
resolves `Base2` in the scope that calls `.AST` and gives
`Trait::Is(type => Type::Simple(...))`. The names the caller declares as types are
now installed for the `is NAME` decision (`CALLER_TRAIT_TYPES`), apart from the
caller terms that make a bare `Base2` a `Term::Name` (#15031).
