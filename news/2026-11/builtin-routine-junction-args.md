# `elems` and `join` treat Junction arguments as rakudo does

`elems` and `join` were on the list of routines whose Junction arguments the
call path never autothreads, so each handled a Junction itself, differently
from rakudo:

- `elems(any((1,2),(3,)))` counted the Junction as one item (`2`); rakudo's
  `sub elems($a)` autothreads to `any(2, 1)`.
- `join(",", any(1,2))` stringified each eigenstate (`any("1", "2")`);
  rakudo's `List.join` hands a lone Junction element back as it is,
  `any(1, 2)`.
- A Junction separator was stringified into the result
  (`"1any(,, ;)2"`); rakudo's `Str() $separator` autothreads the join.

`elems` now goes through the signature-driven autothreader, and `join(...)`
and `.join` share one helper for the Junction cases (#18221).
