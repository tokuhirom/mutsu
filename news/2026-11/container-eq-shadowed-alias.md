# `=:=` tells an alias from a shadowing variable of the same name

`=:=` decides two named operands by following `:=` aliases by name. After
`my $b := $a`, a shadowing inner `my $a` is spelled like the outer `$a` the
alias names, so `$b =:= $a` inside the inner block answered `True`, though
they are different containers (#18807). Writes were already right, because the
store path follows the slot-keyed bind pairs.

When the names match only through an alias, the comparison now consults those
same slot-keyed bind pairs. It answers `False` when following them back from
each operand ends at different slots.
