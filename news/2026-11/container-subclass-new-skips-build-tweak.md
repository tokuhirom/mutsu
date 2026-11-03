# `.new` of a built-in container subclass does not run BUILD/TWEAK

mutsu ran `submethod BUILD`/`TWEAK` for `class C is Hash` (and `Map`, `Array`,
`List`, `Set`, `Bag`, `Mix`, `SetHash`, `BagHash`) on a plain `.new`, so a TWEAK
that stored entries died with "No such method 'BIND-KEY'" before the hash
storage existed, and a positional argument to a `Map` subclass failed to bind
to the submethod. Rakudo's `new` for those classes builds the object without
`bless`, so the submethods never run there; only an explicit `self.bless` runs
them. The default `.new` now skips them for these classes and `bless` is
unchanged (#15656).
