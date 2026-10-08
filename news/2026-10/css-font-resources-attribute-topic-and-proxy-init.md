# CSS::Font::Resources: attribute topics, Proxy initializers, Proxy lvalue FETCH

Three interpreter gaps found by running the CSS::Font::Resources suite:

- `$_ .= new without $!attr` / `given $!attr { $_ .= uc }` now writes the result back to the
  attribute itself, not only the env mirror (the `AssignExpr` path lacked the self-attribute cell
  write that `$_ = ...` already had).
- `my T $x = <Proxy>` FETCHes the Proxy before the declaration's type check, so
  `my CSS::Module::Property $meta = $carray[$i]` over a `CArray[CStruct]` (whose `AT-POS` returns a
  Proxy) no longer fails against `Any (Proxy)`.
- Assigning through an `is rw` sub that returns a Proxy runs STORE only; the old FETCH after STORE
  fired a second user callback that raku never runs.

The remaining blockers are filed as #12321 (a lexical multi sub cannot call its own family by name
once its scope is gone) and #12322 (private-method Proxy assignment still FETCHes).
