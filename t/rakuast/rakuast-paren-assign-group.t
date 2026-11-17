use experimental :rakuast;
use Test;

# A parenthesized assignment on the right of another assignment keeps its
# parentheses in RakuAST, a `Circumfix::Parentheses` (#14880, the `.DEPARSE`
# corpus's S03-operators/misc.t).

plan 6;

sub deparse($src) { $src.AST.statements[*-1].DEPARSE }

is deparse(Q{my $a; 10 R+= ($a ||= 42)}), '10 R+= ($a ||= 42)', 'a reverse compound assignment';
is deparse(Q{my $a; 10 R+= ($a = 42)}), '10 R+= ($a = 42)', 'over an item assignment';
is deparse(Q{my $a; 10 += ($a ||= 42)}), '10 += ($a ||= 42)', 'a compound assignment';
is deparse(Q{my $b; my $c = ($b = 2)}), 'my $c = ($b = 2)', 'a declaration initializer';

my $a;
10 R+= ($a ||= 42);
is $a, 52, 'the reverse compound assignment still assigns';
is EVAL(Q{my $v; 10 R+= ($v ||= 42); $v}.AST), 52, 'and so does EVAL of its .AST';
