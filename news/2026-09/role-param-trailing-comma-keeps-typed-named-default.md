# A trailing comma no longer drops a role's typed named parameter

`role R[::T = Any, K::R :$r = C,]` lost `$r` entirely: inside the role body it
read as `Nil`, so `$r.new(...)` quietly returned `Nil` too. Two parser gaps
stacked up.

`parse_param_list_inner` accepted a trailing comma only before `)`, so a role
parameter list ending in `,]` failed the normal signature parser and fell back
to a per-part splitter. That splitter recognises the `Type ::Capture` form by
splitting a part on its first `::`, and when the text before it named a
declared type it `continue`d past anything it did not recognise. With a stub
`role K {...}` in scope, `K::R :$r = C` looked like the constraint `K` capturing
a type called `R`, and the parameter vanished without an error.

The trailing comma is now accepted before `]` as well, and the fallback hands a
part that is not really `Type ::Capture` on to the ordinary single-parameter
parser instead of dropping it.

Found through the BTree distribution, whose role is declared as
`role BTree[::ValueType = Any, BTree::Renderer :$gist-renderer = ..., BTree::Renderer :$Str-renderer = BasicStrRenderer,]`
next to modules that forward-declare `role BTree {...}`. Its `.Str` and `.gist`
returned `Nil`; `t/simple-int-trees.rakutest` now passes 5/5 and the record is
green. Pinned by `t/oo/role/parametric-role-trailing-comma-named-default.t`.
