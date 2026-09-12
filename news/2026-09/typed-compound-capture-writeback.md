# Typed captured scalars keep their constraint during user-infix compound assignment

Compound assignment to a typed scalar captured by a nested block now carries
the declared constraint onto the shared cell used for writeback. A user-defined
base infix can no longer store a result that violates the scalar's declared
type.
