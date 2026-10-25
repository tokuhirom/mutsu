# Assigning to a type object dies in an expression too

`my $r = (C = C.new)` printed nothing and carried on, and `say(C .= new)` / `my $r = (C .= new)`
did not parse, while the statement forms died with "Cannot modify an immutable 'C' type object".
The store-target check that rejects a type object, `Nil` or an enum value now lives in one
function (`vm_bareword_store`) that the statement store (`SetGlobal`) and the assignment
expression (`AssignExpr`) share, and `.=` on a bareword type object parses wherever `$x .= m` does.
The statement-level bareword `.=` arm of `simple_expr_stmt` is deleted.
