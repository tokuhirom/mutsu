# An element write below `GLOBAL::<%Z>` creates the package variable

```raku
GLOBAL::<%Z><a>++;
say %GLOBAL::Z;   # was (Any), now {a => 1} as in Rakudo
```

A package stash's sigiled entry is the package variable it names
(`GLOBAL::<%Z>` is `%GLOBAL::Z`). An element write below it compiled the
entry as a stash read, which answers a fresh value when the variable does not
exist yet, so the increment or assignment went into a hash nobody stored.
Such a chain now compiles as the qualified variable, through the same paths
as `%GLOBAL::Z<a>++`, for `%`, `@` and `$` entries of any package stash;
pseudo-packages (`MY::`, `OUTER::`, `PROCESS::`, ...) keep their own
lowering (#18204).
