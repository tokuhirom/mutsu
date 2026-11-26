# Mu, Any, Exception and 55 more built-in classes are built on first lookup

`build_builtin_registry` used to build every built-in class with its own `ClassDef` literal
at start-up, whether the program used it or not. For the 58 classes whose literal has no
attributes, the build is now deferred until the class is first looked up (ADR-19206 slice 2).
They include `Mu`, `Any`, `Exception`, `Promise`, `Channel`, `Lock`, the `Supply` and
`IO::Socket` families, `X::AdHoc` and the `X::TypeCheck` tree.

The literals moved verbatim into `builtin_class_catalogue_core{,_a,_b,_c}.rs`:

- `builtin_class_catalogue_core.rs` holds the class names as a list sorted by name, which the
  compiler checks, so lookup is a binary search.
- The other three files hold one builder per class.

These rows share the `X::` rows' catalogue index space. `ClassTable` therefore builds them on
first lookup, into the same process-wide slots, with no change of its own. When a built-in
exception class is built, its rakudo attributes are still marked for declaration on first
access (#18654, #18956).

Thirteen classes are still built at start-up:

- the classes that have attributes, because their accessor rows go into the method tables at
  start-up;
- `IO::CatHandle`, because `IO::ArgFiles` is cloned from it during the build;
- the generated IO-notification classes.

The move also took ~1,300 lines of literals out of `runtime_init.rs`.

`say "hello"` looks up none of these classes, so it builds none of them. Measured on
`benchmarks/bench-startup.raku` with `MUTSU_JIT=off` on a CI-style release build, against
`main` after #19228:

| | before | after |
|---|---:|---:|
| instructions | 4.41M | 4.07M (-7.5%) |
| of which `build_builtin_registry` | 1.07M | 0.71M |
| minor faults | 996 | 966 |
