# Five fixes from Math::SparseMatrix

The ecosystem roulette drew Math::SparseMatrix, and its test suite exposed
five unrelated gaps:

- **User `postcircumfix:<[; ]>` / `<{; }>`.** A multi-dim subscript on an
  object (`$mat[3;2]`, `$mat[<b c>; <A C>]`) now reaches a user-declared
  operator, with the indices passed as one list. This works both as an rvalue
  and as a call argument, exactly as the single-index `postcircumfix:<[ ]>`
  already did.
- **`-->` after a parameter default.** `method transpose(Bool:D :$clone =
  True-->Math::SparseMatrix)` parsed the default as `True--` followed by `>`,
  and the call died with "postfix:<--> requires mutable arguments". Inside a
  parameter default, `-->` is now the return-type arrow.
- **Cool numeric methods on List/Array and Map/Hash.** `.round`, `.floor`,
  `.ceiling`, `.truncate`, `.abs`, `.sign`, `.sqrt`, `.log`, `.exp`, `.conj`,
  `.narrow` and `.is-int` now numify the aggregate to its element count, as
  Cool does in raku. This is what `%h.nodemap(*.round)` relies on: `nodemap`
  does not descend into a nested Hash.
- **A List literal holds an immutable List element by value.** `(@x[0], 1)`
  with a List `@x` held a container around `@x[0]`, so `.head` of it no longer
  flattened into a slurpy. A List element has no container, so the literal now
  holds the value. An Array element is still aliased through its container.
- **`%!attr = @res.tail`.** A method that hands back an Array element's
  container, assigned to a hash attribute, was coerced as one opaque key. It
  now assigns the Hash the container holds.

Of the suite's 20 test files, 18 now pass under mutsu (7 did not before).
`t/07` fails intermittently on random data under rakudo too. `t/29` needs the
native helper library built.
