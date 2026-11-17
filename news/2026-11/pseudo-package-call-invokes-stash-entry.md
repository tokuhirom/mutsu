# A call `PSEUDO::name(...)` invokes the pseudo-stash entry

A call whose name is qualified by a pseudo-package (`CORE`, `SETTING`, `UNIT`,
`OUR`, `LEXICAL`, `OUTERS`, `DYNAMIC`, `CALLER`, `CALLERS`, `CLIENT`, joining
`MY` and `OUTER`) now looks up the stash entry `PSEUDO::<name>` with the
*sigilless* key and invokes it, as rakudo does:

- `constant foo = 3; OUR::foo(1)` and `UNIT::foo(1)` die with
  `No such method 'CALL-ME' for invocant of type 'Int'`.
- `CORE::uc("x")` and `SETTING::lc("X")` die because only `&uc` is in CORE.
  They used to report `Could not find symbol '&uc' in 'GLOBAL::CORE'`, naming
  the pseudo-package as a package nested under `GLOBAL`. rakudo dies on them
  too, with an internal `is_dispatcher` error on a `BOOTCode`; `&CORE::uc("x")`
  is the way to call the core routine, on both.

`GLOBAL::`, `PROCESS::` and `COMPILING::` calls, bare `PSEUDO::name` terms and
type names (`CORE::Int`, `CORE::X::AdHoc`) are unchanged. Fixes #13162.
