# Compound assignment in a closure keeps a rebound `$x` bound

A lexical that a closure captures and that the frame later rebinds with `:=`
is boxed in a *binding cell*, whose content is the variable's current
container (#9237). The fused compound-assignment path (`+=`, `*=`, `~=` …)
stepped that binding cell itself, overwriting the binding with the bare new
value, so the variable lost its alias: `my $x = 1; my $s = sub { $x *= 3 };
my $y = 7; $x := $y; $s(); say $y` printed `7` (raku: `21`).

`++`/`--` had the same defect; it was fixed alongside #10827 and is the shape
#10826 reported — sibling blocks of one frame share a slot for a same-named
`my`, so a `$x := $y` in one block broke `$x++` on an unrelated `$x` in a
later block's named sub.

Both read-modify-write paths now step the cell `Interpreter::value_cell_of`
returns: the binding cells peeled down to the cell that holds the value.
