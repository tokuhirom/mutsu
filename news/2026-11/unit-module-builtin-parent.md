# A class in a `unit module` can inherit from `Nil`, `Failure` and other built-ins

The compiler qualified every `is` parent that was not on a hand-written list of core
types into the enclosing package, so `unit module M; class NOT-SET is Nil` looked for
`M::Nil` and died with "cannot inherit from 'M::Nil' because it is unknown" (LibXML::Class's
`LibXML::Class::Types`). The list is now the runtime's `BUILTIN_PARENT_TYPES` table, the
same one the parent validation uses. A module-defined class named like a built-in
(`class Version`) still wins, as on rakudo.

Also fixed: `Sub ~~ Nil` for a user subclass of `Nil` (`class NS is Nil`) answered
`False`; rakudo answers `True`.
