use v6;
use Test;

plan 17;

# A plain `$x is copy` positional parameter stays writable on the light call
# path and never aliases the caller's variable.
sub bump($x is copy) { $x = $x + 1; $x }
my $a = 10;
is bump($a), 11, 'is copy parameter can be assigned';
is $a, 10, 'the caller variable is untouched';
is bump(bump(1)), 3, 'nested calls each get their own copy';

sub appender($s is copy, $t is copy = 'z') { $s ~= $t; $s ~= '!'; $s }
is appender('a'), 'az!', 'two is copy parameters, one defaulted';
is appender('a', 'b'), 'ab!', 'is copy parameters with both supplied';

# A readonly outer parameter of the same name must not leak into the copy.
sub inner($x is copy) { $x = 5; $x }
sub outer($x) { inner(1) + $x }
is outer(2), 7, 'a same-named readonly parameter does not leak into an is copy one';

# Typed `is copy`.
sub typed(Int $n is copy) { $n = $n * 2; $n }
is typed(21), 42, 'typed is copy parameter';
my $str = 'x';
throws-like { typed($str) }, X::TypeCheck::Binding::Parameter, 'the type is still checked';

# A plain (non-copy) parameter is still readonly, in the same file.
sub ro($x) { $x = 1 }
my $three = 3;
dies-ok { ro($three) }, 'a plain parameter stays readonly';

# An Array is shared by an `is copy` scalar (the container is copied, not the
# array), exactly as in rakudo.
sub push-to($x is copy) { $x.push(9); $x.elems }
my @arr = 1, 2;
is push-to(@arr), 3, 'push through an is copy scalar sees the array';
is @arr.elems, 3, 'the array itself is shared, as in rakudo';

# Reassigning the copy to another value does not disturb the caller's array.
sub reassign($x is copy) { $x = [7]; $x.elems }
is reassign(@arr), 1, 'reassigning the copy';
is @arr.elems, 3, 'the caller array is intact';

# Called in a loop (the shape the light-call cache serves).
my $sum = 0;
$sum += bump($_) for ^20;
is $sum, 210, 'repeated calls through the call cache';

# A constant default is shared by every call: writing to the copy must not
# change what the next call sees.
sub defaulted($n is copy = 5) { $n++; $n }
is defaulted(), 6, 'defaulted is copy parameter, default used';
is defaulted(1), 2, 'defaulted is copy parameter, value supplied';
is defaulted(), 6, 'the constant default is not modified by an earlier call';
