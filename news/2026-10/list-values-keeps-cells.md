# `List.values` keeps the containers of a List built from variables

`for @l.values { $_ = 9 }` over `my @l := ($a, $b)` now aliases `$a` and `$b`
as raku does, instead of dying with "Cannot assign to an immutable value".
The lazy positional view (`ListGen::Positional`) hands out an immutable
List's slots as stored rather than decontainerizing them; nothing is promoted,
so a List of plain items stays read-only. Closes #10396.
