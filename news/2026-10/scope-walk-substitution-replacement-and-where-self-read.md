# The scope walk reads a `s///` replacement and rejects a variable read in its own `where`

Two of the three compile-time scope errors that [#10553](https://github.com/tokuhirom/mutsu/issues/10553)
recorded rakudo reporting and mutsu missing; the third turned out not to be one.

## `s/pat/$y/` is a reference of `$y`

```raku
my $y = "a";
{ $_ = "a"; s/a/$y/; my $y = 2 }
# rakudo: Lexical symbol '$y' is already bound to an outer symbol   mutsu: ok
```

The outer-redeclaration walk (`src/parser/outer_redecl/`) never saw the `$y`: the replacement of
a quote-form substitution is kept as `qq` source text (`Expr::Subst { replacement: String, .. }`)
and parsed again where the substitution runs. The walk now parses the same text once, with the
parse the runtime uses (`parse_dispatch::parse_qq_interpolation`), and visits the result in the
current scope — for `s///` and `S///`, scalars, arrays and hashes. The throw-away parse feeds only
the reference check; the node keeps its text. What stays legal, as in rakudo: a variable declared
earlier in the same scope, a replacement that reads only `$_`, an escaped `\$y`, and a `{ $y }`
code block (a nested code object).

## `my $x where { $x ... }` reads a binding that does not exist

```raku
my $x where { $x > 0 } = 5;
# rakudo 2026.07: Variable '$x' is not declared    rakudo 2026.09: Cannot use variable $x in declaration to initialize itself
```

A declaration's `where` clause is parsed before the variable is introduced, so a read of the
variable there names no binding. mutsu accepted it and ran the block with the not-yet-assigned
variable. The walk now reports `X::Syntax::Variable::Initializer` (the 2026.09 spelling; 2026.07 says
`X::Undeclared`, and both are `X::Comp`) when the clause reads its own variable and no enclosing
scope declares one — a clause that declares its own `my $x`, or a declaration nested in a scope
that already has `$x`, resolves normally, as in rakudo.

## The third case is not an error any more

`my $x = -> $a = $x { $a }` (a pointy-block parameter default that reads the variable being
initialized) is rejected by rakudo 2026.07 but **accepted by 2026.09**, which is what mutsu does.
It is left alone: the newer rakudo is the reference, and the older behavior was the bug.

## Found next door

`my $y = 2; my $x where { $y > 0 } = 5` fails at run time in mutsu: the block reads an enclosing
`$y` as `Any`, because a declaration's `where` is lowered to an anonymous subset that runs without
the declaring scope's lexicals. Filed as [#10732](https://github.com/tokuhirom/mutsu/issues/10732).

## Tests

`t/vm/scope/outer-redeclaration-positions.t` (+7 rows): the three substitution forms that must throw
and the four that must not. `t/vm/scope/declaration-initializer-self-scope.t` (+7 rows): the four
`where` self-reads that must throw and the three that must not. All rows pass under rakudo 2026.07
unchanged (the S/// rows sit before the existing `subset S` row, whose type name rakudo 2026.07 leaks
past its `EVAL` and would otherwise read `S/a/$y/` as a type).
