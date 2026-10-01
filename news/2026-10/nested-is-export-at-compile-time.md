# Nested `is export` declarations are exported at compile time

Rakudo applies `is export` while compiling a declaration, so a declaration at
any depth of a package is exported, even if the code around it never runs. That
covers a routine body, a branch, a bare block, and a nested module or class.
mutsu exported only what registered before the importer copied the export
table. So `module M { sub f { sub g is export { } } }; import M` failed with
`No exports found for module: M`, and a module file's
`sub g { constant kk is export = 5 }` left `kk` a bare string for the importer
(#10543).

- **Routines in inline modules.** The CHECK-time prepass that installs an
  inline package's routines now also collects `is export` routines nested in
  code, and exported routines in class bodies. Each one is exported from every
  package that lexically encloses it, as Rakudo's `@*PACKAGES` walk does. So
  `module M { module N { sub g is export { } } }` exports `g` from both `M` and
  `M::N`.
- **`constant`, `enum` and lexical classes.** An exported one nested in code
  gets a compile-time registration. A copy of the declaration goes into a
  `BEGIN` block placed before the statement of the unit or package body that
  contains it, and the BEGIN prologue (ADR-0134) runs it in source order. The
  declaration in place still registers the same symbols when its code runs. The
  enum type, its keys, and a `my class` keep their identity.

Not covered yet:

- A nested declaration whose initializer names a lexical of its enclosing code
  is left to run time.
- Lexical classes exported from inline modules (#10557).
- A nested exported sub that closes over its block's `my` variables still reads
  them as Nil after an inline import (#10559).
