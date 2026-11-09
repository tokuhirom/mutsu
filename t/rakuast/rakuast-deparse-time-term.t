use Test;
plan 4;
is Q[say time].AST.DEPARSE.chomp, "say time", '`time` is a named term, not a call';
is Q[my $x = now - time].AST.DEPARSE.chomp, "my \$x = now - time", '`now - time`';
is Q[time].AST.statements[0].expression.^name, "RakuAST::Term::Named", 'Term::Named';
my $t = EVAL(Q[time].AST);
ok $t ~~ Int && $t > 0, 'EVAL of the AST still calls time';
