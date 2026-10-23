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
as `Spelling::Origin` wrappers. An origin is exact or absent, never a guess:
a note belongs to the innermost statement that holds it, and a fingerprint's
ranges are attached in source order only when that statement has no more
notes than terms of it, so a note left by an alternative the parser abandoned
cannot move a range onto another term. Terms the parser keeps a spelling for
count too, so heredocs and word lists get their origins as well. The RakuAST
conversion takes the wrappers off before it matches shapes and
looks each term's range up by slot, or by fingerprint for the copies it makes.
The range is stored in a new `origin` member of `RakuAstNode` that does not
take part in node equality, so hand-built nodes (which have none) still
compare equal to parsed ones. Executing parses note nothing and pay nothing.
