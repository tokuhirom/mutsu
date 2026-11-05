# `Stash.new` keeps its class through element stores

`Stash.new` built an instance with no backing storage, and the hash-subclass delegate
looked up its MRO only in the class registry (a builtin `Stash` has no registered MRO),
so `$s<x> = 1` fell through to the plain-Hash coercion and replaced the Stash with a
`Hash`. The constructor now gives the instance the same `__mutsu_hash_storage` a user
`is Hash` subclass has, and the delegate checks consult the builtin catalog's MRO when
the class is not registered. Fixes #16346.
