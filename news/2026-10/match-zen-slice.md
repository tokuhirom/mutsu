# `$<>` is the Match itself, subscriptable in strings and s/// replacements

The zen slice `$<>` / `$/<>` of a Match used to stringify it, and inside a double-quoted string
or an `s///` replacement `"$<>[0]"` was read as the named capture `""` followed by a literal
`[0]`. Both now follow Rakudo: `$<>` is the Match, so `$<>[0]` indexes its positional captures.
Found by making the `DB::ORM::Quicky` test suite pass (3 of 3 files now at parity), whose SQLite
column introspection relies on `s/ ... (.*) ... /$<>[0]/`.
