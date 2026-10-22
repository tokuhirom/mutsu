# NativeCall's exported constants bind as pad terms

NativeCall's `is export` constants (`CArray`, `Pointer`, `size_t`, ...) used to
reach an importer only through env aliases, because NativeCall declares a
`sub EXPORT` and the parse-time scan of such a module recorded just the hook's
literal keys. The scan now also lists the module's own `is export` variables and
constants, so they bind as lexical terms of the importing unit (ADR-8751,
#14140). In the census over `t/` this was the largest remaining group of env
alias writes.

Three readers that only knew the env were fixed along the way:

- A routine declared after `use NativeCall` resolves the aliases in its
  signature from its declaring frame. Registration then dropped that result and
  re-read the plan's spelling as written. Now only a signature-less routine's
  implicit `*@_`/`*%_` comes from the plan, so `sub f(CArray[int32], size_t)`
  keeps `NativeCall::Types::size_t`.
- A top-level routine that names a term boxes it into a shared cell. Type-alias
  resolution and `constraint_type_value` now read through that cell, so a
  `has Pointer $.p` field and a `--> Pointer` return check still see the type.
- The attribute default path's `CA2[uint16]` skip and the hoisted-twin
  signature comparison from the work-in-progress branch were dropped: the fixes
  above make them unnecessary.
