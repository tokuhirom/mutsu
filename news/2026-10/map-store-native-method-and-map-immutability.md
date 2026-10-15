# Native `Map.STORE`; a Map (subclass) has no push/append and refuses `%m[0] = ..`

Found running ValueMap (all 23 assertions of its suite now match rakudo):

- `Map.^lookup('STORE')` now finds the native method (it answered `Mu`), so a
  user `is Map` class can delegate to it (`STORE self, @values, :INITIALIZE,
  :DECONT`, as ValueMap does). Called on an `is Map`/`is Hash` instance it
  stores into the instance's backing hash and answers the instance.
- `Hash.push`/`append` rows no longer apply to a `Map` (nor an `is Map`
  subclass), whose Rakudo type has neither method.
- `%vm[0] = 42` on an `is Map` instance raised no error and replaced the
  instance with a Hash holding its internals; it is now refused as an immutable
  write, as for a plain `Map`.
- The tied-variable `STORE` that populates a Map subclass kept no declared type
  on the backing hash, so the subclass silently became mutable; it now keeps the
  `Map` tag.
