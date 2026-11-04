# `my $p := $q` shares a Proxy's STORE

`my $q := Proxy.new(...); my $p := $q; $p = 7` did not run the Proxy's STORE,
and it also destroyed the Proxy in `$q` (a later `$q = 8` no longer stored
either): the bind boxed `$q`'s slot into a shared `ContainerRef` cell, and the
`ContainerRef` write-through of a plain `=` ran before the Proxy check, which
only looked at a slot holding a bare Proxy. The slot store now looks inside the
cell first: a cell that holds a Proxy with a STORE routes the assignment through
it, and a cell holding the `.VAR` Proxy object stays the readonly-variable error
from #16299 ([#16306](https://github.com/tokuhirom/mutsu/issues/16306)).
Regression test: `t/vm/writeback/proxy-bind-from-proxy-variable-stores.t`.
