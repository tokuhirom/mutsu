# `$which does R` on an ObjAt/ValueObjAt reblesses it in place

`$which does role { ... }` on the `ObjAt`/`ValueObjAt` that `.WHICH` returns built a mixin
copy, so a value class that caches its identity object (ValueClass's `$!WHICH`) never saw
the role from another holder. The object is now reblessed into `ValueObjAt+{R}` in place,
as Rakudo does for any object, and a role-mixed `ObjAt` keeps rendering its identity text
(`.Str`, `.gist`, `.raku`). `ValueObjAt` is also an inheritable built-in parent now.
ValueClass `t/01-basic.rakutest` goes 37 -> 52 ok (rakudo 58); the container-initializer
double wrap and the `is rw` value-class check are still open on #14588.
Pinned in `t/oo/role/which-does-role-in-place.t`.
