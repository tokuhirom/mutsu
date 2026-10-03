# A `multi` in expression position evaluates to its candidate

Upstream NativeCall's `EXPORT` does:

```raku
my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) { ... };
Map.new('&trait_mod:<is>' => $native_trait.dispatcher);
```

mutsu could not parse `multi NAME(...)` (without `sub`) in term position.

`multi sub NAME` did parse, but it evaluated to an unnamed copy of the body: `.name` was empty and
`.dispatcher` was `Nil`. Worse, the copy was declared inside a desugared block, so the declaration
never joined the multi in the enclosing scope.

A `multi` declaration in expression position is now a `DoStmt(SubDecl)`. It registers the candidate
in the current scope and evaluates to that candidate:

- The candidate is named and answers `.multi`.
- It runs only its own signature.
- Its `.dispatcher` is the multi it joined.

`do multi sub ...` behaves the same way. This is a slice of ADR-11203 (#11205).
