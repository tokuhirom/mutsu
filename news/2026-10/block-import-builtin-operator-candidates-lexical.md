# Operator candidates imported in a block stay in that block

```raku
sub outside($a, $b) { $a + $b }
my &add;
{
    use Points;              # exports multi infix:<+>(Pt, Pt)
    say ($p + $q).gist;      # Pt(3)
    say outside($p, $q);     # dies: no candidate outside the block
    &add = -> $a, $b { $a + $b };
}
say add($p, $q).gist;        # Pt(3): the closure keeps the import
```

A `use` inside a block that imports new candidates for a built-in operator
(`multi infix:<+>`, `multi prefix:<->`, the comparisons, `cmp`, `eqv`,
`min`/`max`) used to put them in the global routine table for as long as the
block ran. Any routine the block called saw them too, and a closure made in
the block lost them once the block had exited. Now they are bound in the
block's own lexical scope, as rakudo binds them. The operator, its compound
assignment, `[+]`, `»+«`, `&infix:<+>` and a `sort` comparator in the block
all see them. A call that none of them accepts goes to the built-in, and a
`callsame` in a candidate reaches the built-in too (ADR-8751 stage 2).
