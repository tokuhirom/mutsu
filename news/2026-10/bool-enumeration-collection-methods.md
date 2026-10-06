# Bool collection views follow its enum values

`Bool.keys`, `values`, `kv`, `pairs`, `antipairs`, `minpairs` and `maxpairs` now reflect the `True` and `False` enumeration entries. `Bool.unique` returns a singleton `Seq`, matching Rakudo. Fixes #12089.
