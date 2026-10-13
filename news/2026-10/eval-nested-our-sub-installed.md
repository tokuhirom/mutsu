# An EVAL installs the `our` routines nested in its routines up front

Since #13091 a program installs an `our sub` declared inside one of its
routines when it is compiled, so `GLOBAL::helper()` works before the routine
ever runs. An `EVAL`'d snippet is a compilation unit too, but did not run that
prepass:

```raku
say EVAL q[sub f { our sub ghelper { 6 } }; GLOBAL::ghelper()]
# raku and now mutsu: 6; mutsu died with "Could not find symbol 'ghelper' in 'GLOBAL'"
```

The `EVAL` path now runs the same inline-package-routine prepass right after
ordering the snippet's BEGIN prologue (#13137).
