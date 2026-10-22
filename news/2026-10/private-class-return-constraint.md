# A module-private class in a return constraint is a type, not an EVAL expression

`module M { my class C {}; our sub f(--> C:D) is export { C.new } }` died after the routine
ran, with a generic parse error: the return constraint, already resolved to the lexical
class's registry identity (`M::C\0<id>:D`), was classified as a definite return *expression*
and handed to EVAL. The identity holds a NUL, which never occurs in source, so
`is_definite_return_spec` now reads it as a type; the return check against the private class,
including `X::TypeCheck::Return` for a wrong value, is unchanged (#14145).
