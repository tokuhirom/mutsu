# `self{$k}:exists` subscripts `self`

`self{$k}:exists` used to parse as the invocant-colon form of
`name { block }: args`, that is `{$k}.self(exists)`. The result was a Block,
which is always true. OrderedHash's `keys` method
(`@!keys.grep: { self{$_}:exists }`) therefore reported all 62 slots as
present, and every Trie node looked like it had many children.

Two rules now hold in the parser branch for "a bareword followed by a block":

- A nullary term keyword (`self`, `Mu`, `now`, ...) is a complete term, so a
  brace after it subscripts the term instead of opening a listop's block
  argument.
- A colon that opens a colonpair (`}:exists`, `}:k`) is an adverb, not that
  form's invocant colon.

With both fixed, Trie's suite passes.
