# `keyof` of a class composing `Associative[V, K]`

`class CH is Hash does Associative[Cool, DateTime] {}; CH.new.keyof` was
`X::Method::NotFound`: only `.of` read the composition. The role-spelling reader that answers
`.of` now takes the argument index, and `keyof` reads the key (second) parameter, defaulting
to `Str(Any)` for `Associative[V]`, for the instance, the type object, a subclass and a
parametric role that passes the parameter on (#12989).
