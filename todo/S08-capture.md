# S08 - Capture and Argument Lists

Reference: `old-design-docs/S08-capture.pod`

Covers the Capture type, argument binding, and multidimensionality.

---

## Capture Type

- [ ] Capture as first-class type
- [ ] Positional vs named argument separation
- [ ] `\(...)` capture constructor
- [ ] `|` prefix to create capture from object
- [ ] Capture binding to signatures

---

## List vs Capture

- [ ] List keeps all items visible
- [ ] Capture splits positional/named
- [ ] Conversion between List and Capture

---

## Multidimensionality

- [ ] Nested captures preserve dimensionality
- [ ] Multidimensional indexing (`$a[1;1;1]`)
- [ ] Semicolon separates dimensions in subscripts

---

## Context Deferral

- [ ] Deferred evaluation via capture
- [ ] Avoid premature context coercion
- [ ] Replace Perl 5 `wantarray` semantics
