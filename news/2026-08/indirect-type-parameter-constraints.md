# Indirect type constraints in parameter signatures are parsed

Parameter signatures now accept `::(EXPR)` indirect type constraints, including
the common `::(q<TypeName>)` form used to avoid forward-reference ordering
issues in roles and classes. The constraint is retained in the signature and
resolved during type checking, so methods using it are no longer rejected or
silently dropped from role composition.
