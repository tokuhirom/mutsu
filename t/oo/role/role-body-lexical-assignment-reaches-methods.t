use Test;

# An assignment statement in a role body reaches the `my` variable the role's
# methods closed over: the statement used to write a stray `R::b` because the
# body's package qualified the name (#14616). Expected values are the `raku`
# output.

plan 8;

role R1 { my $b = 1; $b = 6; method m { $b } }
class A1 does R1 {}
is A1.new.m, 6, 'an assignment after a declaration with an initializer';

role R2 { my $b; $b = 6; method m { $b } }
class A2 does R2 {}
is A2.new.m, 6, 'an assignment after a bare declaration';

role R3 { my $b = 1; method m { $b }; $b = 6 }
class A3 does R3 {}
is A3.new.m, 6, 'an assignment after the method';

role R4 { my $b = 1; for 1..3 { $b = $_ } method m { $b } }
class A4 does R4 {}
is A4.new.m, 3, 'an assignment inside a loop';

role R5 { my $b = 1; $b += 5; method m { $b } }
class A5 does R5 {}
is A5.new.m, 6, 'an in-place update is unchanged';

role R6 { my @b = 1; @b.push: 2; method m { @b.raku } }
class A6 does R6 {}
is A6.new.m, '[1, 2]', 'a mutated array is unchanged';

role R7 { my $b = 1; $b = 6; method m { $b }; method n { $b = 9 } }
class A7 does R7 {}
my $o = A7.new; $o.n;
is $o.m, 9, 'a method assignment is seen by another method';

role R8 { my $b = 1; $b = 6; method m { $b } }
class A8 does R8 {}
class A9 does R8 {}
is A8.new.m + A9.new.m, 12, 'each composition sees it';
