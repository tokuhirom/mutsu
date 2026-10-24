use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A reverse (`R-=`) and a set-operator (`(|)=`) compound assignment are the
# typed compound-assignment node: `.AST` matches rakudo 2026.09, and the tree
# lowers to what the parsed program does (ADR-10723 Stage 2, #14948). This
# file passes under both mutsu and raku.

plan 11;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $reverse = last-statement(Q[my $x; my $y; $x R-= $y]);
is $reverse.infix.^name, 'RakuAST::MetaInfix::Reverse', 'R-= is the reversed infix';
is $reverse.infix.infix.^name, 'RakuAST::MetaInfix::Assign', 'around the compound assignment';
is last-statement(Q[my $x; my $y; $x R= $y]).infix.infix.^name, 'RakuAST::Assignment',
    'R= reverses the plain assignment';

my $set = last-statement(Q[my $s; $s (|)= 1]);
is $set.infix.^name, 'RakuAST::MetaInfix::Assign', 'a set operator is a compound assignment';
is $set.infix.infix.operator, '(|)', 'over the set infix as written';

# The trees run like the parsed program.
is EVAL(Q[my $x = 1; my $y = 10; $x R-= $y; "$x $y"].AST), '1 9', 'R-= assigns to its right operand';
is EVAL(Q[my $x = 1; my $y = 10; $x R= $y; "$x $y"].AST), '1 1', 'R= assigns the left to the right';
is EVAL(Q[my @a = 5, 5; my $y = 1; $y R+= @a[1]; @a.join(",")].AST), '5,6', 'R+= writes into a subscript';
is EVAL(Q[my $s = set(1); $s (|)= 2; $s.keys.sort.join(",")].AST), '1,2', 'a set union';
is EVAL(Q[my %h; %h<a> = set(1, 2); %h<a> (&)= 2; %h<a>.keys.join(",")].AST), '2',
    'a set intersection into a subscript';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $x; my $y; $x R-= $y].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
