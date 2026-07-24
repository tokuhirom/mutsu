unit module WhereConstraintMod;

# A `where`-constrained named parameter keeps the routine on the interpreter
# carrier (it is not eligible for the on-the-fly compile), so calling it runs
# the constraint through `check_named_param_where_constraint`.
sub constrained(Int $n, :$c! where .so) is export { $n }

# Same signature shape without the constraint, as the control.
sub unconstrained(Int $n, :$c) is export { $n }
