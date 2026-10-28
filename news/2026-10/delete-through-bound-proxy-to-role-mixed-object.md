# `:delete` through a variable bound to an rw Proxy reaches the object

`$d<a><b>[1]:delete` compiles to `my $t := $d<a><b>; $t[1]:delete`. When the
subscript chain is served by an object that mixes in a role with `AT-KEY` and
`DELETE-POS` (Red's JSON items), `$d<a><b>` is the `Proxy` its rw `AT-KEY`
returns, so `$t` was bound to the `Proxy`: the named `:delete` looked at the
`Proxy`/cell it found in the variable instead of the object it fetches, and
never called `DELETE-POS`/`DELETE-KEY`. The variable is now read through its
`:=` cell and a `Proxy` is fetched first, so a nested `:delete` reaches the
object (Red t/87 "Nested array deletion shifts elements").
