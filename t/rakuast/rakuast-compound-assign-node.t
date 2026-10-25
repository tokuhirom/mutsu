use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A compound assignment is the typed compound-assignment node: `.AST`
# matches rakudo 2026.09, and the tree lowers to what the parsed program does
# (ADR-10723 Stage 2, #14948). This file passes under both mutsu and raku.

plan 20;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $plain = last-statement(Q[my $x; $x += 1]);
is $plain.infix.^name, 'RakuAST::MetaInfix::Assign', 'a compound assignment is a MetaInfix::Assign';
is $plain.infix.infix.operator, '+', 'over the plain infix';

my $zip = last-statement(Q[my $x; $x Z+= 1]);
is $zip.infix.infix.^name, 'RakuAST::MetaInfix::Zip', "a scalar's Z+= keeps its zip metaoperator";
is $zip.infix.infix.infix.operator, '+', 'over the plain infix';

my $bracketed = last-statement(Q[my $x; $x [[+]]= 1]);
is $bracketed.infix.infix.^name, 'RakuAST::BracketedInfix', '[[+]]= keeps its brackets';
is $bracketed.infix.infix.infix.^name, 'RakuAST::BracketedInfix', 'both of them';

is last-statement(Q[my @a; @a[0] += 1]).left.^name, 'RakuAST::ApplyPostfix', 'a subscript target as written';
is last-statement(Q[my $x; say($x //= 1)]).args.args[0].infix.infix.operator, '//',
    'a compound assignment in an argument';

# The trees run like the parsed program.
is EVAL(Q[my $x = 1; $x += 2; $x].AST), 3, 'a scalar';
is EVAL(Q[my $x = 1; $x Z+= 2; $x].AST), 3, "a scalar's Z+=";
is EVAL(Q[my $x = 1; $x [+]= 2; $x].AST), 3, 'a bracketed infix';
is EVAL(Q[my @a = 1, 2; @a[1] *= 10; @a.join(",")].AST), '1,20', 'a subscript';
is EVAL(Q[class C { has $.n is rw }; my $c = C.new(n => 1); $c.n += 4; $c.n].AST), 5, 'a method call';
is EVAL(Q[class D { has $.s is rw = "a" }; my $d = D.new; $d.s ~= "z"; $d.s].AST), 'az',
    'a method call written as a statement';
is EVAL(Q[class F { has $.n is rw }; my $f = F.new; $f.n //= 7; $f.n //= 9; $f.n].AST), 7,
    'a short-circuit operator on a method call';
is EVAL(Q[my ($a, $b) = 1, 1; (True ?? $a !! $b) += 5; "$a $b"].AST), '6 1', 'the branch a ternary selects';
is EVAL(Q[my $x = 3; say($x //= 7) for (); $x //= 7; $x].AST), 3, 'a defined scalar keeps its value';
is EVAL(Q[my $x; my $r = ($x //= 7); "$x $r"].AST), '7 7', 'an undefined scalar takes the value';
is EVAL(Q[my $x = 1; $x += 1 and $x += 10; $x].AST), 12, 'a loose and runs after the assignment';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $x; $x Z+= 1].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
