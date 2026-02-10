# S10 - Packages

Reference: `old-design-docs/S10-packages.pod`

Covers package declarations, nesting, and autoloading.

---

## Package Declarations

- [ ] `package Bar { ... }` syntax
- [ ] No bareword package names (must predeclare or use `::`)
- [ ] `our package` for current scope (default)
- [ ] `my package` for lexical scope
- [ ] Anonymous packages: `package { ... }` or `package :: { ... }`
- [ ] Traits set with `is`

---

## Package Nesting

- [ ] `A::B::c` auto-creates empty packages `A` and `A::B`
- [ ] Search from innermost lexical to outermost
- [ ] Then current package upward to `GLOBAL`
- [ ] `PROCESS::` namespace shared by interpreters

---

## Autoloading

- [ ] `multi CANDO(Container, Type, $name, *%args)` replaces AUTOLOAD
- [ ] Called when name lookup fails
- [ ] Return autovivifiable proxy or `Nil`
- [ ] `method CANDO` for classes
- [ ] `submethod CANDO` for class-specific
