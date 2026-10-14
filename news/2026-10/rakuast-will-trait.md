# RakuAST renders `will` traits on variables

A variable declaration can carry phaser traits that run with `$_` bound to
the container:

```raku
my $x will begin { $_ = 3 };
my @a will begin { .push(1, 2) };
```

`.AST` refused them. A declaration now renders its `will` traits as
`RakuAST::Trait::Will` nodes with the phase name and the written block, as
rakudo does, and EVAL of the tree runs each block in its phase.

Two more `t/` files pass under `MUTSU_RAKUAST=1`. The work also exposed #13330
(`will leave` in a nested block of an EVAL string never runs).
