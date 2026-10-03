# Trie: `self{$k}:exists`, Seq-to-`@` method dispatch, and user Associative adverbs

Trie's suite went from dying at its seventh test to passing 67 of 70. That took
five fixes.

- **`self{$k}:exists` subscripts `self`.** It used to parse as the
  invocant-colon form of `name { block }: args`, i.e. `{$k}.self(exists)`,
  which yields a Block that is always true. OrderedHash's
  `keys` (`@!keys.grep: { self{$_}:exists } }`) therefore listed all 62 slots.
  A nullary term keyword (`self`, `Mu`, ...) now never heads that form, and a
  colon that opens a colonpair (`}:exists`) is an adverb, not its invocant
  colon.
- **A `Seq` binds to `@` in multi-method dispatch as a List**, as multi-sub
  dispatch already ranked it. Trie's `delete(@arr)` / `delete(Str() $key)`
  pair used to send `$key.comb` back to the `Str()` candidate, and the
  re-coerced string grew until memory ran out. Both dispatchers now share
  `seq_as_positional_distance`.
- **A private call written in a class works when it runs lazily on a
  type-object invocant**: `T.all` returning `gather { self!all }`. It used to
  die with "does not trust GLOBAL" once the Seq was read outside the method.
  The lexical-`self` trust fallback now also accepts a type-object `self`.
- **`:k`/`:v`/`:kv`/`:p` on a user Associative** ask the requested keys of
  `EXISTS-KEY` and read them through `AT-KEY`. They used to snapshot the whole
  object through its `keys` method first, which Trie does not have. A gather
  value is read in full.
- **`$o[*-1]` on a user Positional** that only answers `AT-POS` is computed
  against `.elems` instead of reading Nil.
