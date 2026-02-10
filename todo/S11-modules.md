# S11 - Modules and Compilation Units

Reference: `old-design-docs/S11-modules.pod`

Covers module declarations, importation, and versioning.

---

## Compilation Units

- [x] `use` for loading modules
- [ ] `need` for loading without importing
- [ ] `require` for runtime loading
- [ ] `EVAL` for dynamic code (implemented separately)
- [ ] CompUnitRepo interface for repositories

---

## Module Declarations

- [ ] `unit module Foo;` syntax
- [ ] `module Bar { ... }` block syntax
- [ ] Module traits with `is`
- [ ] Anonymous modules

---

## Exportation

- [ ] `is export` trait on declarations
- [ ] Tagsets: `:DEFAULT`, `:ALL`, `:MANDATORY`, custom
- [ ] `EXPORT` subroutine for dynamic exportation
- [ ] Inner packages add exports to outer scopes

---

## Importation

### Compile-time
- [x] `use Module` basic import
- [ ] `use Module <&func @var>` selective import
- [ ] Lexical scope import (not package scope)
- [ ] `:MY<>`, `:OUR<>` explicit scoping
- [ ] `:EXPORT` for re-export
- [ ] `use` = `need` + `import`

### Runtime
- [ ] `require Module <symbol>` runtime import
- [ ] `require "/path/Module.pm"` from filename
- [ ] `require ::($module_name)` dynamic module name

---

## Versioning

- [ ] Long name: name + auth + ver + api
- [ ] `use Dog:auth(Any):ver(Any)` selection
- [ ] Version range: `use Dog:ver(v1.2.1..v1.2.3)`
- [ ] `use v6` = `use Perl:ver<6.*>`
- [ ] Multiple version coexistence
- [ ] Tie-breaking by API then version

---

## Forcing Perl 6

- [ ] `use v6` switches to Perl 6 mode
- [ ] `unit module/class` implies Perl 6
