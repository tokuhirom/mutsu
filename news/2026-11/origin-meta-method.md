# `.^name` carries origins, and its empty `ArgList` is visited

`$x.^name` is a `Call::MetaMethod`. It had no range, and its empty `ArgList`
(which rakudo's `visit-children` visits) was missing from the walk. The
metamethod call now goes through the same placement as `.name(...)`
(`call_origins::place_call`): the call from its name to its end, and the
`ArgList` empty right after the name, just inside the parentheses, or from the
first to the last argument. `empty_fields::materialized` also stops when a
class lists a visited field it declares no empty node for, which would have
recursed forever. On the 54-file differential, nodes rakudo has and mutsu
lacks went from 2365 to 2241, with no new mismatches. Part of #15780.
