use Test;

plan 6;

# A method named with a quote-words form keeps the written colonpair in
# `.AST` and `.DEPARSE` (identical under rakudo).
my $ast = Q|class WrittenA { method postcircumfix:«[ ]»($i) { 1 } }|.AST;
like $ast.DEPARSE, / 'postcircumfix:<<[ ]>>' /, 'french quote form deparses as written';

my $plain = Q|class WrittenB { method postcircumfix:<[ ]>($i) { 1 } }|.AST;
like $plain.DEPARSE, / 'postcircumfix:<[ ]>' /, 'angle form deparses as written';
unlike $plain.DEPARSE, / '<<' /, 'angle form stays an angle quote';

my $spaced = Q|class WrittenC { method infix:<< + >>($o) { 1 } }|.AST;
like $spaced.DEPARSE, / 'infix:<< + >>' /, 'spaced double-angle form deparses as written';

# The tree still declares the method under its canonical name.
EVAL($ast);
ok ::('WrittenA').^find_method('postcircumfix:<[ ]>').defined, 'EVAL of the tree declares the method';
EVAL($spaced);
ok ::('WrittenC').^find_method('infix:<+>').defined, 'spaced form declares the canonical name';
