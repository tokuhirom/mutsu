# Package-stash index built lazily: startup back to its pre-#9845 cost

The index that tells whether a spelled `Foo::Bar::` stash names an existing
package (#9845) was maintained eagerly from `Symbol::intern_global`, so every
process paid for it on each of the thousands of qualified names interned at
startup, although almost no program reads a stash. It is now built on the first
`names_under_package` lookup by walking the append-only symbol table, and each
later lookup folds in only the symbols interned since.

`bench-startup` (callgrind, release): 11,868,160 Ir / 16,301 allocations →
10,575,555 Ir / 14,757 allocations (-10.9% / -1,544), the level measured with the
eager recording removed (#10228).
