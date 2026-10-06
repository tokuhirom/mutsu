# An `our sub` can rebind a file-scope lexical with `:=`

`my $buf := [1, 2]; our sub rebind() { $buf := []; $buf.elems }` returned 2
and left `$buf` unchanged: the escaped-`our`-sub capture cell was a plain cell,
so the rebind landed in the call's env overlay. The cell is now a binding cell
for a name the sub rebinds, shared with the declaring frame's slot, and the
rebind seats the new binding inside it, as for a mainline named sub (#12130).
