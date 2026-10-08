use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# Assignments whose target is not a plain variable keep their written shape in
# `.AST` and round-trip through it: an assignment to a literal, a compound
# assignment through a parenthesised compound assignment or a ternary, and the
# statement sequence of `$(a; b)`.
plan 20;

# --- LITERAL = RHS ----------------------------------------------------------
my $literal = Q[120 = 3].AST;
my $expr = $literal.statements[0].expression;
is $expr.^name, 'RakuAST::ApplyInfix', 'an assignment to a literal is an ApplyInfix';
is $expr.left.^name, 'RakuAST::IntLiteral', 'its left side is the literal';
is $expr.infix.^name, 'RakuAST::Assignment', 'its infix is the plain assignment';
is $expr.right.^name, 'RakuAST::IntLiteral', 'its right side is the value';
throws-like { EVAL $literal }, X::Assignment::RO,
    'EVAL of an assignment to a literal throws X::Assignment::RO';

throws-like { EVAL Q[sub note-it($v) { $v }; "a" = note-it(5)].AST }, X::Assignment::RO,
    'a quoted string is an immutable target as well';

# --- (COMPOUND) OP= RHS -----------------------------------------------------
my $nested = Q[my $a; ($a //= 42) += 10].AST;
my $outer = $nested.statements[1].expression;
is $outer.infix.^name, 'RakuAST::MetaInfix::Assign',
    'a compound assignment through a parenthesised compound assignment keeps its metaoperator';
is $outer.left.^name, 'RakuAST::Circumfix::Parentheses',
    'the parenthesised compound assignment stays on the left';
is EVAL(Q[my $a; ($a //= 42) += 10; ($a //= 42) += 10; $a].AST), 62,
    'EVAL runs both steps against the same container';
is EVAL(Q[my $a = 1; (($a += 2) *= 3) -= 1; $a].AST), 8,
    'a chain of parenthesised compound assignments evaluates';
throws-like { EVAL Q[my $a := 42; ($a //= 42) += 10].AST }, X::Assignment::RO,
    'a containerless inner value is immutable for the outer operator';

# --- (COND ?? A !! B) OP= RHS ------------------------------------------------
my $ternary = Q[my %c; my $r = 0 ?? 1 !! %c<k> //= 2].AST;
is $ternary.statements[1].expression.initializer.expression.infix.^name,
    'RakuAST::MetaInfix::Assign',
    'a trailing compound assignment takes the whole ternary as its target';
is EVAL(Q[my %c; my $r = 0 ?? 1 !! %c<k> //= 2; "$r {%c<k>}"].AST), '2 2',
    'the else branch is written through the selected container';
is EVAL(Q[my %c; my $r = 1 ?? 3 !! %c<k> //= 4; "$r {%c<k>.defined}"].AST), '3 False',
    'the unselected branch is untouched';

# --- $(STMT; STMT) -----------------------------------------------------------
my $stmts = Q[my $s = $( my $x = 3; $x + 1 )].AST;
my $item = $stmts.statements[0].expression.initializer.expression;
is $item.^name, 'RakuAST::Contextualizer::Item', 'a statement list in $( ) is an item contextualizer';
is $item.target.^name, 'RakuAST::StatementSequence', 'over a statement sequence';
is $item.target.statements.elems, 2, 'holding both statements';
is EVAL(Q[my $s = $( my $x = 3; $x + 1 ); $s].AST), 4, 'EVAL evaluates the statements in order';
is EVAL(Q[my $a = 1; my $s = $( temp $a = 23; $a ); "$s $a"].AST), '23 23',
    'a temp inside $( ) is scoped to the enclosing block';
is EVAL(Q[my $v = "x$( my $y = 3; $y * 2 )z"; $v].AST), 'x6z',
    'the statement sequence of an interpolated $( ) evaluates';
