# S09 - Data Structures

Reference: `old-design-docs/S09-data.pod`

Covers arrays, hashes, typed/compact data, and autovivification.

---

## Lazy Lists

- [x] List contexts are lazy by default (gather/take)
- [ ] `eager` to force non-lazy
- [x] `flat` to guarantee flattening

---

## Sized Types

- [ ] Named with bit counts: `int1`, `int32`, `int64`
- [ ] `num16`, `num32`, `num64`
- [ ] `complex64`, `complex128`
- [ ] `buf8`, `buf16`, `buf32`, `buf64`

---

## Arrays

### Standard Array Indexing
- [x] Square brackets `[]`, zero-based
- [x] Contiguous indices

### Fixed-size Arrays
- [ ] `my @array[size]` declaration
- [ ] Fail on out-of-range access
- [ ] Autoextending with `my @array[*]`

### Typed Arrays
- [ ] `my num @nums` or `my @nums of num`
- [ ] Element type enforcement

### Compact Arrays
- [ ] Low-level typed arrays (bit, int, num)
- [ ] Contiguous memory storage

### Multidimensional Arrays
- [ ] Semicolon-separated dimensions: `my @array[4;2]`
- [ ] Mixed fixed/autoextending dimensions
- [ ] Indexed by semicolon lists
- [ ] Autoextending dimensions (`*`, `**`)

### User-defined Array Indexing
- [ ] Custom indices via curly braces: `my @dwarves{1..7}`

### Subscript Features
- [x] `*` as array length: `@array[*-1]`
- [ ] `:v` adverb (omit unallocated elements)
- [ ] Zen slice `@array[]`
- [ ] Slicing with virtual views (copy-free aliasing)

---

## Hashes

- [x] Standard hash access (`%hash{key}`, `%hash<key>`)
- [ ] Multiple dimensions in hashes
- [ ] Fixed key sets
- [ ] Typed key dimensions
- [ ] Autosorted hashes (`is ISAM` trait)
- [x] Iteration via `.keys`, `.values`, `.kv`, `.pairs`

---

## Compact Structs

- [ ] Classes with value-type attributes as structs
- [ ] C alignment/padding rules
- [ ] Serialization mapping

---

## Junctions

- [x] Four varieties: any `|`, all `&`, one `^`, none
- [x] Auto-parallelize when passed to routines
- [x] Results recombined in same junction type

---

## Autovivification

- [ ] Only on read-write container binding
- [ ] `push`, `.[]` autovivify Array
- [ ] `.{}` autovivifies Hash
- [ ] Value extraction does not autovivify

---

## PDL Support

- [ ] Arrays tied to PDL via `is PDL` trait
- [ ] Tensor operation signatures
