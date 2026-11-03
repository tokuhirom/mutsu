# Reading a CArray (`.list` / `.Array`) no longer clones the role's method table per element

Each `AT-POS` the `CArray[T]` `.list` loop makes asked, through `method_binds_raw_invocant`, for the
mixed-in role's candidates by cloning the whole `RoleDef`'s method table, and then rebuilt the
mixin's `Base+{Role[...]}` type name to resolve the class. The raw-invocant probe now borrows the
role's candidates (`mixin_role_method_defs_any`) and a role-mixed class instance's class name is
memoized by its shared override node (the entry owns the node, so its address cannot be reused).

Callgrind slope of fill (`@c[$_] = 1.5e0`) plus `.Array` on a `CArray[num64]`, profiling build,
second run: 255.6k Ir per element on main -> 219.6k (-14%). Refs #16215.
