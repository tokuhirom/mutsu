# Origins of a declaration with a loose tail

`my $fh = open $file orelse die "x $file"` and the other declarations and
assignments a loose operator continues (`my $x = 1 and 2`, `$x = 1 and 2 or 3`,
`my ($a, $b) = 1, 2 and 3`) now place their head in RakuAST as rakudo does: the
`VarDeclaration` or assignment, its `Initializer`, and the loose operator's
`Infix`. The head is also converted before the tail, so a variable
interpolated into the tail's string no longer takes the range of the head's
same-named variable — a wrong origin, worse than none (#18578).
