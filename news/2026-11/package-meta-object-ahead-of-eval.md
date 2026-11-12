# A RakuAST package node has its type before `EVAL`

In rakudo the type a `RakuAST::Class` (`Role`, `Grammar`, `Package`,
`Module`) declares exists as soon as the node is built, and `EVAL` composes
that same type. mutsu had no `meta-object` for these nodes at all: the call
fell through to the parameter row and died with "EVAL does not yet support
lowering RakuAST::Class".

Now (ADR-17472):

- `.AST` records its unit's top-level layout on the unit's
  `RakuAST::Origin::Source`: which statements are `use`-like and which declare
  a package.
- `meta-object` of a package node not yet declared declares it on demand: the
  unit's preceding `use` statements plus the declaration run like a tree
  `EVAL`, and the type is recorded on the source under the node's offset.
- `EVAL` of the unit adopts a recorded type instead of declaring a second one,
  so a `.HOW.set_name` made in between survives, and `meta-object` after
  `EVAL` answers the type the `EVAL` declared.

```raku
my $ast = 'class Foo { method m { 42 } }; Foo'.AST;
my $type = $ast.statements[0].expression.meta-object;
say $type.m;                        # 42, before EVAL
$type.HOW.set_name($type, 'Renamed');
say $ast.EVAL.^name;                # Renamed
```

EEL 0.3's command line (`eel -e CODE`) renames inline event types exactly this
way before running them (#17472).

A declaration made ahead runs in a unit of its own, so a method that reads a
`my` variable declared earlier in the same unit does not compile there yet;
rakudo declares the type at BEGIN time inside the unit's own scope.
