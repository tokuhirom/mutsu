# `CALLERS::<&sub>` sees a file-level sub of a parameterized role

A package-less `sub` in the body of a role with type parameters is moved into its
compilation unit's private routine table once the role is composed
(`seclude_role_lexical_routine`). The `CALLERS::<&name>` / `CALLERS::OUTERS::<&name>`
lookup only read the routine registry and the unit's import sets, so a routine called
from such a role's method never found the role file's own helper (`False False`
where rakudo prints `True True`). The lookup now reads the unit-private table too.

This is what Red 0.2.7's `Red::ResultAssociative[$of, Red::AST $key-of]` needs for its
`X::Red::NoOperatorsDefined` guard: `Model.^all.classify(...)` answered a bare
`Red::AST::Function` for `.elems` and printed "Red operators not defined while
calling map" before.
