# A role a method trait composes belongs to the method declaration

ADR-11827 phase 3. A method's `MethodDef` now owns the composition cell that its declaration's traits
ran on, and every method object `.^find_method`, `.^lookup` and `.^methods` build reads it. The
name-keyed `ROUTINE_MIXIN_ROLES` table, which re-attached role markers by `"Class::name"` (and so
tagged every candidate of a multi, and lost role arguments), is removed together with
`materialize_routine_mixins*`.
