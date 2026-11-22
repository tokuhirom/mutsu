# A `use` of a missing module is a compile error

Every `use` loads its module at compile time in rakudo, so a module the
repository chain cannot find fails the compilation of the unit that names it:

```
===SORRY!=== Error while compiling main.raku
Could not find NoSuchModule in:
    ...
at main.raku:3
```

mutsu raised the same `X::CompUnit::UnsatisfiedDependency`, but rendered it as
a run-time error with a backtrace (`in block <unit> at main.raku line 3`). The
failing `use` now marks the error as a compile error at its own line. When the
`use` sits inside a loaded module, the error carries that module's file and
source, so the report names the module rather than the program that loaded it.
`EVAL` and `try` still see the typed exception.
