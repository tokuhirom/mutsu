# `.^methods(:all)` ignores the `:all` adverb — returns own methods only

Discovered via the doc-diff harness on `raku-doc/doc/Type/Metamodel/MethodContainer.rakudoc`
(around line 40).

## Minimal repro

```raku
class A {
    method x() { };
}
say A.^methods();      # own methods only
say A.^methods(:all);  # should ALSO include inherited methods
```

- `raku`:
  ```
  (x POPULATE)
  (x POPULATE EXISTS-KEY DELETE-KEY DELETE-POS cache list fmt flat eager serial List Slip Array
  Seq hash Hash Map elems end keys kv values pairs antipairs invert splice pick roll match
  classify categorize reverse combinations permutations join tree push append unshift prepend ...)
  ```
  (167 methods total for `.^methods(:all)` vs. 2 for the plain form)
- `mutsu` (`target/debug/mutsu`):
  ```
  (x)
  (x)
  ```
  Both calls return the same single-element list — `:all` has no effect at all.

## Root cause hypothesis

`.^methods` presumably only ever walks the class's *own* method table
(`Perl6::Metamodel::ClassHOW`'s locally-declared methods) and doesn't check for/honor an `:all`
named argument that should walk the full MRO chain and collect inherited methods too (mirroring
how `.^method` reflection generally needs to distinguish "declared on this class" vs. "visible
via inheritance").

## Affected files (starting point)

- `src/runtime/methods_classhow_dispatch.rs` — wherever `.^methods` is implemented; needs an
  `:all` named-arg check that walks the class's full MRO (`class_mro` or similar helper already
  used elsewhere for method resolution) and unions in each ancestor's own methods, rather than
  just the invocant class's local table.
