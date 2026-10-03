# RakuAST: indexed bind `@a[i] := v` through one parser builder

After the container-trait slice, a desugared `__mutsu_bind_index_value`
marker was among the largest `.AST` refusals: 65 `t/` files. The parser
spells an indexed bind (`@a[0] := $x`, `%h<k> := $y`) as an `IndexAssign`
whose value is that marker around the RHS, so the VM takes bind semantics.
Rakudo 2026.09 models it as a plain `:=` infix over the subscript.

Three parse paths built the marker, each by hand:

- the statement-level `:=` after a subscript;
- the expression-level assignment parser;
- the logical-precedence fast path for `my $c = %h<k> := v`.

They now share `parser::index_bind_expr`. The converter renders the marker
as rakudo's `ApplyInfix(left => <subscript>, infix => Infix(":="), right)`,
and the lowering hands the subscript and value back to the same builder.
The round trip therefore produces exactly the tree the parser does,
including the bind-source metadata the VM reads.

A slice or multi-dimensional bind (`@a[0,1] := …`, `@a[1;1] := …`) flattens
its index to the same list, so the two cannot be told apart and neither
renders. Rakudo does not compile a slice bind either.
