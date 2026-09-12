# Role mixins keep live attributes on non-instance values

Role attributes mixed onto hashes, arrays, native scalars, and wrapped
instances now use a shared live cell owned by the mixin. Compiled method
writes persist across calls, remain visible through public accessors, stay
separate from same-named class or other-role attributes, and are copied by
Raku `.clone` into independent role state.
