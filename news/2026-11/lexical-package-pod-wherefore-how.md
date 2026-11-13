# A lexical package's declarator pod reports the package's metaclass

The `WHEREFORE` of a declarator pod attached to `my module M { }` or `my package P { }` is built before the declaration registers, so it can only name the package by its bare spelling, while a lexical package lives under its storage name (`M\0<decl-id>`, ADR-0047). `.HOW` of that bare-named type object found no package kind and fell back to `ClassHOW`. It now resolves the bare spelling to the one lexical package declared under it, so `$=pod[0].WHEREFORE.HOW ~~ Metamodel::ModuleHOW` holds as in rakudo.

Pod::To::Man renders a package's documentation by that test, and its `t/01-basic.rakutest` now passes 35/35 like rakudo (was 33/35).
