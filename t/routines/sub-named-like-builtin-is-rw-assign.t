use Test;

plan 6;

# A user `sub` named like a builtin (`get`, `lines`) is what a call to that name
# resolves to, in an lvalue position as well as in a plain call.
sub get($x is rw) is rw { $x }
my $v = 1;
get($v) = 5;
is $v, 5, 'get($v) = 5 assigns through the user `sub get`';

sub lines($x is rw) is rw { $x }
my $w = 1;
lines($w) = 8;
is $w, 8, 'the same for `lines`';

get($v) += 2;
is $v, 7, 'a compound assignment through it';

sub word($x is rw) is rw { $x }
my $z = 1;
word($z) = 3;
is $z, 3, 'a name that is not a builtin keeps working';

sub get2($x) { "user:$x" }
is get2(5), 'user:5', 'a plain call of an ordinary user sub';
sub uc($x) { "user-uc:$x" }
is uc("a"), 'user-uc:a', 'a plain call of a user sub named like a builtin';
