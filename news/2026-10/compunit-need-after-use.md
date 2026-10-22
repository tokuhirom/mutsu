# `$*REPO.need` after `use`, and `Str ~~ CompUnit`

`CompUnit::Repository.need` of a unit a `use` already loaded no longer runs the unit's body
again (it died with `Redeclaration of routine`), and a repeated `need` answers the same
compunit even when the calls come from different routines. `'Test'.ACCEPTS($compunit)` now
compares the short-name. With these, CompUnit::Util's `t/01-utils.rakutest` gets from 1 to 7
assertions (#14539); the next gap there is `CompUnit::Handle.unit`.
