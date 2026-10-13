# TOML: walking a table path by rebinding a dynamic variable

The TOML distribution builds a table path with `$*tml := $*tml{$key}` from a
routine that did not declare `$*tml`, and stores each `key = value` through
`my $*ptr := $*tml; $*ptr := $*ptr{$k}; $*ptr = $v`. Four separate gaps made
every table header and every second key in a document land in the wrong place:

- Rebinding a `my`-declared dynamic that shared its cell with another variable
  to a deferred (missing) element wrote through the shared cell and clobbered
  the other variable. It now detaches, as a plain lexical does.
- A by-name read of a dynamic in a bind chain (`GetGlobalDeferred`) keeps the
  deferred element token, so the next key extends the path instead of resolving
  to `Any`.
- A `SetGlobal` rebind whose source is a subscript temp no longer reuses the cell
  an earlier statement's temp of the same number left behind, and a cell holding
  a deferred token is replaced by the next token instead of materializing the
  previous key.
- `$x := $c ?? 1 !! $x<k>` binds the selected branch's element like a plain
  subscript does.

`chr("...")` as a function now numifies a Str with the same routine as
`Str.chr`, so `chr(":16<41>")` is `A`.

TOML's `t/02-invalid.t` passes 66/66 and `t/04-nqp.t` 101/102.
