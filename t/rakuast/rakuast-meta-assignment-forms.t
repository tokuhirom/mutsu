use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# The metaoperator assignments keep their written shape in `.AST` and round-trip
# through it: the reverse assignment (`R-=`), a set operator over `=` (`∪=`) and
# the hyper assignment (`»=»`).
plan 24;

# --- $x R-= $y ---------------------------------------------------------------
my $reverse = Q[my ($a, $b) = 10, 3; $a R-= $b].AST;
my $expr = $reverse.statements[1].expression;
is $expr.infix.^name, 'RakuAST::MetaInfix::Reverse', 'R-= is a reversed infix';
is $expr.infix.infix.^name, 'RakuAST::MetaInfix::Assign', 'over the compound assignment';
is $expr.infix.infix.infix.gist, 'RakuAST::Infix.new("-")', 'of the base infix';
is EVAL(Q[my ($a, $b) = 10, 3; $a R-= $b; "$a $b"].AST), '10 -7',
    'the right operand is assigned';
is EVAL(Q[my ($a, $b) = 10, 3; my $r = ($a R-= $b); $r].AST), -7,
    'the expression has the assigned value';
is EVAL(Q[my $z = 5; 10 R+= $z; $z].AST), 15, 'a literal on the left is read';
throws-like { EVAL Q[my $f = "x"; $f R~= "lit"].AST }, X::Assignment::RO,
    'a literal on the right cannot be assigned';
my $plain = Q[my ($a, $b); $a R= $b].AST;
is $plain.statements[1].expression.infix.infix.^name, 'RakuAST::Assignment',
    'R= reverses the plain assignment';
is EVAL(Q[my ($a, $b) = 1, 2; $a R= $b; "$a $b"].AST), '1 1', 'R= copies to the right';

# --- %h<k> ∪= VALUE ----------------------------------------------------------
my $set = Q[my %h; %h<a> ∪= 3].AST;
my $set-expr = $set.statements[1].expression;
is $set-expr.infix.^name, 'RakuAST::MetaInfix::Assign', 'a set operator assignment is a compound assignment';
is $set-expr.infix.infix.gist, 'RakuAST::Infix.new("∪")', 'over the set infix';
is $set-expr.left.^name, 'RakuAST::ApplyPostfix', 'with the subscript on the left';
is EVAL(Q[my %h; %h<a> ∪= (3, 4).Set; %h<a>.keys.sort.join(",")].AST), '3,4',
    'EVAL stores the union through the subscript';
is EVAL(Q[my $s = set(1); $s (|)= set(2); $s.keys.sort.join(",")].AST), '1,2',
    'a plain variable stores the union back';
is EVAL(Q[my $t = bag(1); $t ⊎= bag(1); $t{1}].AST), 2, 'the Unicode spelling is kept';

# --- (LVALUES) »=» VALUE -----------------------------------------------------
my $hyper = Q[my ($x, $y); ($x, $y) »=» 5].AST;
my $h = $hyper.statements[1].expression;
is $h.infix.^name, 'RakuAST::MetaInfix::Hyper', 'a hyper assignment is a hyper infix';
is $h.infix.infix.^name, 'RakuAST::Assignment', 'over the assignment';
is $h.left.^name, 'RakuAST::Circumfix::Parentheses', 'with the list of lvalues on the left';
is EVAL(Q[my ($x, $y); ($x, $y) »=» 5; "$x $y"].AST), '5 5', 'a scalar is broadcast';
is EVAL(Q[my ($x, $y, $z); (($x, $y), $z) »=» 9; "$x $y $z"].AST), '9 9 9',
    'nested lvalues are distributed';
is EVAL(Q[my ($x, $y); ($x, $y) «=« (1, 2); "$x $y"].AST), '1 2', 'a list is distributed';
is EVAL(Q[my $r = ((my $a, my $b) »=» (1, 2)); "$a $b"].AST), '1 2',
    'declarations in the target are kept';
is EVAL(Q[my @a = 1, 2; @a »=» 7; @a.join(",")].AST), '7,7', 'a single array target';
throws-like { EVAL Q[my ($x, $y); ($x, $y) »=« (5, 6, 7)].AST }, X::HyperOp::NonDWIM,
    'a length mismatch without dwim throws';
