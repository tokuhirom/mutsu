# `PSEUDO::name` reads the pseudo-stash, bare or called

A long name whose head is a pseudo-package (`CORE`, `SETTING`, `MY`, `OUTER`,
`OUTERS`, `UNIT`, `LEXICAL`, `OUR`, `DYNAMIC`, `CALLER`, `CALLERS`, `CLIENT`)
now means the stash lookup `PSEUDO::<name>` with the *sigilless* key, as in
rakudo:

- `constant foo = 3; say UNIT::foo` prints 3 (it used to die with
  `Could not find symbol '&foo' in 'UNIT'`), and `say CORE::uc` prints `Nil`.
- `CORE::uc("x")`, `SETTING::lc(...)`, `OUR::foo(...)` and the other call forms
  invoke that stash value, so a routine name dies with Nil's `CALL-ME` failure,
  as `MY::helper()` / `OUTER::helper()` already did. Before, they reported
  `Could not find symbol '&uc' in 'GLOBAL::CORE'`, naming the pseudo-package as
  a package nested under `GLOBAL`. (rakudo dies on `CORE::uc("x")` too, with an
  internal `is_dispatcher` error on a `BOOTCode`; `&CORE::uc("x")` is the way to
  call the core routine, on both.)

Type names are unchanged: `CORE::Int`, `CORE::Int:D`, `CORE::Array[Int]` and
`CORE::X::AdHoc` still resolve to the type. `GLOBAL::`, `PROCESS::` and
`COMPILING::` keep their own resolution. Fixes #13162.
