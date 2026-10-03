# Trie: object subscripts, nested gathers, private calls and a dispatch regression

Drawing `Trie` from the ecosystem roulette turned up six bugs. With all of
them fixed, its `t/02-trie` passes 70/70 under mutsu, matching rakudo; before,
it failed at test 19 and then hung.

- **`self{...}:adverb` parses as a subscript.** `self{$_}:exists` was read as
  the invocant-colon call `{$_}.self(:exists)`, which is how its OrderedHash
  dependency's `.elems` became 62. A `{` glued to an identifier is a
  postcircumfix, and a nullary term such as `self` is never a listop head.
- **A Seq argument prefers `@arr` over a `Str()` coercion candidate** in
  method dispatch. This regressed when `Seq` stopped being `Positional`: the
  method ranker scored the Seq as unrelated to `@`. It now ranks the Seq as
  the List it binds as, the same way multi-sub dispatch already did. Without
  this, Trie's `delete("ab")` recursed forever.
- **`self!private` in a `gather` on a type object** is allowed when the gather
  is consumed lazily from another package. The lexical `self` check now
  accepts a type object as well as an instance.
- **Slice adverbs on an Associative object** (`$o<k>:k`, `:v`, `:kv`, `:p`)
  now ask `EXISTS-KEY` and `AT-KEY` for the given keys, as rakudo does.
  Before, the adverb path snapshotted the whole object through `.keys`, and a
  class without its own `keys` answered with `Any.keys`, giving `AT-KEY(0)`.
- **A `gather` nested in a list is pulled when the list is read.** This covers
  one held in a variable and the results of a hyper method call
  (`@nodes>>.all`). Such a Seq used to read as empty. It now renders as
  `(5)` inside `say` and `.gist` too.
- **`$obj[*-1]` on an object with its own `AT-POS`** calls the WhateverCode
  with the object's `.elems`.
