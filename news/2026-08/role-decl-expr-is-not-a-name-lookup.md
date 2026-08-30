# A role declaration expression now yields the role it just declared

A named `role` declaration used in expression position could return an
unrelated same-named role from another scope. Registration stored the new role
under its package-qualified name, but the compiler then performed a fresh bare
name lookup to obtain the expression value. An outer bare binding could win
that lookup.

Role registration now records its actual qualified registry key. A dedicated
opcode pushes that exact role group immediately after registration, after
which the existing candidate conversion selects the individual parametric role
declaration. `t/role-decl-expr-value.t` covers the cross-compilation-unit EVAL
case and confirms that the outer role is unchanged.
