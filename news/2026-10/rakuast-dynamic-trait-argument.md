# A variable's `is dynamic(ARG)` keeps its argument in RakuAST

`my $x is dynamic(42)` rendered `Trait::Is(name => dynamic)` with the argument
dropped, so `EVAL` of the tree and a deparse lost it, and a hand-built trait
with an argument was refused (#14881). The parser keeps the written argument
for the RakuAST view (the trait itself stays the declaration's `is_dynamic`
flag), and the conversion and the lowering carry it, as rakudo 2026.09's tree
does.
