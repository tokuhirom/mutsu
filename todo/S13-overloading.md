# S13 - Overloading

Reference: `old-design-docs/S13-overloading.pod`

Covers operator overloading, multiple dispatch, and type casting.

---

## Multiple Dispatch for Operators

- [x] `multi sub` declarations for type-based dispatch
- [ ] Full type-based overloading for binary operators
- [ ] Commutative handling for mixed types
- [ ] Multi import/export semantics

---

## Operator Overloading Syntax

- [ ] `multi sub infix:<+>(MyType $a, MyType $b) { ... }`
- [ ] `multi sub prefix:<->(MyType $a) { ... }`
- [ ] `multi sub postfix:<++>(MyType $a is rw) { ... }`
- [ ] `multi sub circumfix:<{ }>(MyType $a) { ... }`
- [ ] `multi sub postcircumfix:<[ ]>(MyType $a, $idx) { ... }`

---

## Fallbacks

- [ ] `AUTODEF` for undefined routines
- [ ] Deep vs shallow interpretations
- [ ] `%?DEEPMAGIC` hash for auto-generation

---

## Type Casting

- [ ] `postcircumfix:<( )>` for callable coercion
- [ ] `postcircumfix:<[ ]>` for array coercion
- [ ] `postcircumfix:<{ }>` for hash coercion
- [ ] Sigil-dot short forms (`&.`, `@.`, `%.`)
- [ ] Standard coercion methods: `.Str`, `.Num`, `.Int`, `.Bool`
