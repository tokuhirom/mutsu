use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# A declaration followed by a loose operator is one expression with the
# declaration as its leftmost operand: `my $x = 1 and 2` is `(my $x = 1) and 2`,
# and `my $x = 1, 2, 3` is a comma list whose first element is the declaration.
plan 17;

# --- my $x = 1 and 2 ---------------------------------------------------------
my $and = Q[my $x = 1 and 2].AST;
my $expr = $and.statements[0].expression;
is $expr.^name, 'RakuAST::ApplyInfix', 'a word-logical tail makes the statement an infix application';
is $expr.left.^name, 'RakuAST::VarDeclaration::Simple', 'whose left operand is the declaration';
is $expr.left.initializer.^name, 'RakuAST::Initializer::Assign', 'with its initializer';
is $expr.infix.gist, 'RakuAST::Infix.new("and")', 'and whose infix is the word operator';
is $expr.right.^name, 'RakuAST::IntLiteral', 'applied to the tail';

is EVAL(Q[my $x = 1 and 2; $x].AST), 1, 'the declaration keeps its own value';
is EVAL(Q[my $x = 0 or 5; $x].AST), 0, 'the tail applies to the assignment, not to the initializer';
is EVAL(Q[my $r = (my $y = 1 and 7); $r].AST), 7, 'the whole expression yields the tail';
is EVAL(Q[my @a = 1, 2 andthen 3; @a.join(",")].AST), '1,2', 'a list initializer stays whole';

# --- chains and a comma tail -------------------------------------------------
my $chain = Q[my $x = 1 or 2 and 3].AST.statements[0].expression;
is $chain.infix.gist, 'RakuAST::Infix.new("or")', 'the looser operator is outermost';
is $chain.left.^name, 'RakuAST::VarDeclaration::Simple', 'with the declaration still leftmost';

my $comma = Q[my $x = 1, 2, 3].AST.statements[0].expression;
is $comma.^name, 'RakuAST::ApplyListInfix', 'a comma tail makes a list infix application';
is $comma.operands.elems, 3, 'of three operands';
is $comma.operands[0].^name, 'RakuAST::VarDeclaration::Simple', 'the first being the declaration';

# --- an assignment to an existing variable -----------------------------------
my $plain = Q[my $z; $z = 0 or 9].AST.statements[1].expression;
is $plain.^name, 'RakuAST::ApplyInfix', 'a plain assignment takes a tail as well';
is EVAL(Q[my $z; $z = 0 or 9; $z].AST), 0, 'and keeps the assigned value';
is EVAL(Q[my $x = 5, 6, 7; $x].AST), 5, 'the comma tail does not extend the initializer';
