# `===` against a Junction honours a user `WHICH` on every eigenstate

`$key === none($ident)` compared the eigenstates by per-object id until a plain `===` had happened to
compute their `WHICH`, so `Assigns.without` in SQL::Abstract kept the conflict-target columns in an
upsert's `DO UPDATE SET` list (#16778). The identity warm now visits a Junction's eigenstates.
