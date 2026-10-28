# A class body's typed object hash keeps its key type when the body is split

A class whose subs declare a nested class has its body split by the BEGIN prologue: `my
%as{Any:U} = ((Str) => ...)` becomes a declaration (run ahead) plus an assignment (run in the
`PackageScope`). The scope entry hid the body's `__mutsu_hash_key_type::%as` lane and nothing
put the recorded one back, so the assignment treated the hash as `Str`-keyed and stringified
the type-object keys with two "Use of uninitialized value" warnings. Loading Path::Finder 0.4.7
printed them (#15247); the scope entry now restores the package's own recorded lanes.
