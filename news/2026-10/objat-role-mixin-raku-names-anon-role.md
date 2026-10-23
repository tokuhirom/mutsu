# A role-mixed `ObjAt`/`ValueObjAt` names an anonymous role `<anon|N>` in `.raku`

`$w does role { }` on a `ValueObjAt` (what a value-class `.WHICH` hands out) rendered its `.raku` with the compiler's internal key, `ValueObjAt+{__ANON_ROLE_0__}.new("a|b")`. It now goes through `user_facing_type_name`, as the other mixin renderings do, and prints `ValueObjAt+{<anon|1>}.new("a|b")` like Rakudo (#14588).
