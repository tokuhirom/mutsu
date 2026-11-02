use Test;

# #15933: a plain scalar is its own 0th element, and `.AT-POS(0)` hands back
# its container, so it can be assigned through.

plan 6;

my $a = 42;
is-deeply $a.AT-POS(0), 42, 'AT-POS(0) reads the scalar';
is-deeply ($a.AT-POS(0) = 666), 666, 'assigning through AT-POS(0) answers the value';
is-deeply $a, 666, 'the variable holds it';

sub f { my $s = "x"; $s.AT-POS(0) = "y"; $s }
is f(), "y", 'a Str variable inside a routine';

my @arr = 1, 2, 3;
@arr.AT-POS(1) = 20;
is-deeply @arr, [1, 20, 3], 'an Array element is unchanged behaviour';
my %h = a => 1;
%h.AT-KEY('a') = 2;
is %h<a>, 2, 'a Hash element is unchanged behaviour';
