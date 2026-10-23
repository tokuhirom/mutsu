# RakuAST term nodes carry their origin

`.AST` now gives expression nodes the source range the parser read them from,
as Rakudo does: a literal, a variable, a method call, a parenthesized list or
the argument of a listop reports `.origin` with grapheme `from`/`to` offsets
into the unit's source, so tools that walk a tree with `visit-children` (such
as the `Coverable` distribution) see every line that starts a term.

The parser makes many decisions by looking at the shape of a term it has just
read, so a term cannot carry its range while the unit is still being parsed.
A parse that keeps spelling therefore notes each term's range together with a
structural fingerprint of the term, and once the whole unit (including its
post-parse passes) is finished the ranges are attached to the matching terms
as `Spelling::Origin` wrappers, searched within the statement each was written
in. The RakuAST conversion takes the wrappers off before it matches shapes and
looks each term's range up by slot, or by fingerprint for the copies it makes.
The range is stored in a new `origin` member of `RakuAstNode` that does not
take part in node equality, so hand-built nodes (which have none) still
compare equal to parsed ones. Executing parses note nothing and pay nothing.
