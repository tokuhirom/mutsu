use v6;
use Test;

plan 9;

my $*A = 1;
sub rd { $*A }
my $t = 1;

if $t { my $*A = 4; is rd(), 4, "visible inside the if branch"; }
is $*A, 1, "an if branch's my \$*A is gone after it";

with $t { my $*A = 5; is rd(), 5, "visible inside the with branch"; }
is $*A, 1, "a with branch's my \$*A is gone after it";

given 1 { my $*A = 7; is rd(), 7, "visible inside the given body"; }
is $*A, 1, "a given body's my \$*A is gone after it";

sub g1($c) { my $r = do with $c { my $*A = 8; 9 }; $*A }
sub g2($c) { my $r = do if $c { my $*A = 8; 9 }; $*A }
sub g3($c) { my $r = do with $c { my $*A = 8; 9 }; $r }
is g1(1), 1, "do with in a sub leaves the dynamic variable alone";
is g2(1), 1, "do if in a sub leaves the dynamic variable alone";
is g3(1), 9, "and still yields the branch value";
