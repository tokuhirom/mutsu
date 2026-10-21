# A module's qualified type alias leaves the program's frame env

A class or role a module declares under a qualified name it does not own,
such as `class X::Imported::Boom` inside `unit module M` (stored as
`M::X::Imported::Boom`), is reached by the program under the written spelling.
That alias used to be bound in the env the module body loads in. That env is
the importing program's own scope, so every copy-on-write copy of a program
frame copied it.

A module mainline now records the alias in `ModuleToplevel::package_symbols`
(ADR-0084 §2 group 2, #7817), as it already did for enum values. This covers
the written spelling of a nested qualified class, the identity binding of a
qualified name whose storage name differs, and a compiler-qualified source
alias. The type readers consult that table: `has_type`, the bareword type
probe (`resolve_bareword_type_name`) and the smiley bareword
(`X::Imported::Boom:U`).

With `use Cro::HTTP2::RequestParser`, a `Promise(supply { whenever … })`
iteration copies 406 env entries instead of 470. A bare program copies 42.
