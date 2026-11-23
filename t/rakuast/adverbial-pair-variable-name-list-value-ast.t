use Test;
use experimental :rakuast;

plan 10;

# A declared name whose colonpair value is a list of integers (`my $y:a[1,2]`)
# is spelled back by `.AST`, canonicalized by its value as rakudo does, and an
# `EVAL` sees the declaration under that canonical name (#18612).

is EVAL(Q[my $y:a[1,2] = 3; $y:a<1 2>].AST), 3, '.AST.EVAL of :a[1,2]';
is EVAL(Q[my $z:a(1,2) = 4; $z:a<1 2>].AST), 4, '.AST.EVAL of :a(1,2)';
is EVAL(Q[my $w:a[1,'x'] = 5; $w:a<1 x>].AST), 5, 'an integer and a string';
is EVAL(Q[my $v:a[-3,20] = 6; $v:a<-3 20>].AST), 6, 'a negative integer';

is Q[my $y:a(1) = 3].AST.statements[0].expression.desigilname.canonicalize, 'y:a<1>',
    '.canonicalize';
is Q[my $y:a[1,2] = 3].AST.DEPARSE.trim, 'my $y:a<1 2> = 3', '.DEPARSE';

is EVAL(Q[my $y:a(1) = 3; $y:a<1>]), 3, 'a plain EVAL of :a(1)';
is EVAL(Q[my $y:a[1,2] = 3; $y:a<1 2>]), 3, 'a plain EVAL of :a[1,2]';
is EVAL(Q[my $y:a(1+1) = 3; $y:a<2>]), 3, 'an evaluated value';
throws-like { EVAL Q[my $y:a(1) = 3; $y:a<2>] }, X::Undeclared,
    'another value is still undeclared';
