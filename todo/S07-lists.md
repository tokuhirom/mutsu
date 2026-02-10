# S07 - Lists and Iteration

Reference: `old-design-docs/S07-lists.pod`

Covers list types, iteration semantics, and the single argument rule.

---

## User-level Types

### List
- [x] Immutable lists from `infix:<,>`
- [x] Indexing and `.elems`
- [ ] Strict immutability enforcement

### Array
- [x] Mutable subclass of List
- [x] Auto-growth
- [x] `push`, `pop`, `shift`, `unshift`
- [x] `splice`
- [ ] Full Array container semantics

### Seq
- [ ] One-shot sequence type
- [ ] Lazy evaluation
- [ ] `.cache` method
- [ ] Single iteration enforcement
- [ ] Seq vs List distinction

### Slip
- [ ] Flattens into surrounding List
- [ ] `.Slip` coercion method
- [ ] `prefix:<|>` operator for slipping

### HyperSeq
- [ ] Parallel sequence processing
- [ ] `.hyper` and `.race` methods

---

## The Single Argument Rule

- [ ] `@`-sigil items flatten, `$` items don't
- [ ] Consistency across operators
- [ ] Scalar container prevents flattening

---

## Iterable Role

- [ ] `.iterator` method
- [ ] `.flat` method for flattening
- [ ] Iterator protocol (pull-one, push-exactly, etc.)

---

## Lazy Evaluation

- [x] Lazy lists (gather/take)
- [ ] `lazy` contextualizer
- [ ] `eager` contextualizer
- [ ] "Mostly eager" semantics for list assignment
- [ ] `.is-lazy` method

---

## List Operations

- [x] `flat` (flatten nested lists)
- [ ] `list` operator (impose list context)
- [ ] `item` operator (impose item context)
- [ ] `.tree` method (level-sensitive mapping)
- [ ] `|` prefix for capture context

---

## Sorting

- [x] `.sort` method
- [x] `.sort` with custom comparator
- [ ] Stable sort guarantee
