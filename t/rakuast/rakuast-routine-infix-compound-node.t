use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A compound assignment over an infix with no built-in compound operator -- a
# word operator (`mod=`) or a user-declared infix (`+%+=`) -- is the typed
# compound-assignment node: `.AST` matches rakudo 2026.09, and the tree lowers
# to what the parsed program does (ADR-10723 Stage 2, #14948). This file
# passes under both mutsu and raku.

plan 12;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $word = last-statement(Q[my $n; $n mod= 5]);
is $word.infix.^name, 'RakuAST::MetaInfix::Assign', 'mod= is a MetaInfix::Assign';
is $word.infix.infix.operator, 'mod', 'over the word infix';
my $user = last-statement(Q[sub infix:<+%+>($a, $b) { $a + $b * 2 }; my $x; $x +%+= 5]);
is $user.infix.^name, 'RakuAST::MetaInfix::Assign', 'a user infix compound is a MetaInfix::Assign';
is $user.infix.infix.operator, '+%+', 'over the user infix';
is last-statement(Q[my $n; say($n mod= 5)]).args.args[0].infix.infix.operator, 'mod',
    'in an argument';
is last-statement(Q[my $n; 1 and $n mod= 5]).right.infix.infix.operator, 'mod',
    'as the operand of a looser operator';

# The trees run like the parsed program.
is EVAL(Q[my $n = 7; $n mod= 5; $n].AST), 2, 'mod= as a statement';
is EVAL(Q[my $n = 7; my $r = ($n mod= 5); $r].AST), 2, 'mod= in parentheses';
is EVAL(Q[my $n = 7; 1 and $n mod= 5; $n].AST), 2, 'mod= after `and`';
is EVAL(Q[sub infix:<+%+>($a, $b) { $a + $b * 2 }; my $x = 1; $x +%+= 5; $x].AST), 11,
    'a user infix compound as a statement';
is EVAL(Q[my @a = 7, 8; @a[1] mod= 5; @a.join(",")].AST), '7,3', 'mod= on an element';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $n; $n mod= 5].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
