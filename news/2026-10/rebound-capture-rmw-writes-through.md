# `$x++` in a closure keeps a rebound `$x` bound

A lexical that a closure captures and that the frame later rebinds with `:=`
is boxed in a *binding cell*, whose content is the variable's current
container (#9237). The read-modify-write paths — `++`/`--` in both forms and
the fused compound assignments (`+=`, `*=`, `~=` …) — stepped that binding
cell itself, overwriting the binding with the bare new value. The variable
then lost its alias: `my $x = 1; my $s = sub { $x++ }; my $y = 7; $x := $y;
$s(); say $y` printed `7` (raku: `8`), and `$x` read the wrong value.

Because sibling blocks of one frame share a slot for a same-named `my`, a
`$x := $y` in one block was enough to break `$x++` on an unrelated `$x` in a
later block's named sub (#10826).

`Interpreter::value_cell_of` peels binding cells to the cell holding the
value, and both read-modify-write paths now step that cell.
