use Test;

# Pins the semantics of the `VarRef` value representation — the wrapper the
# caller tags an lvalue argument with (`OpCode::WrapVarRef`) so the binder can
# alias the caller's container instead of copying its value.
#
# It used to be encoded as a `Capture` carrying the magic named keys
# `__mutsu_varref_{name,value,index}`; it is now a first-class `Value` variant.
# Every consumer below reads it through a different path (signature binding,
# capture construction, pair construction, `:=`, multi dispatch, `.VAR`), so
# these are the cases that would break if a path missed the migration.

plan 14;

# is rw -- the binder aliases the caller's scalar container
sub inc($x is rw) { $x++ }
my $a = 1; inc($a); is $a, 2, 'is rw writes back to the caller variable';

# is raw
sub raw(\r) { r }
my $b = 5; is raw($b), 5, 'is raw binds the container and reads through it';

# an array variable passed into a sub shares the container
my @arr = 1, 2, 3;
sub pushit(@a) { @a.push(9) }
pushit(@arr); is @arr.join(','), '1,2,3,9', 'array argument shares the container';

# \($c) captures the *container*, so writing through the capture hits $c
my $c = 1;
my $cap = \($c);
$cap[0]++;
is $c, 2, 'capture \\($c) aliases the variable container';

# named capture element
my $d = 7;
my $cap2 = \(:$d);
is $cap2<d>, 7, 'named capture element reads the variable';

# `key => $var` makes the Pair's value share $var's container
my $e = 3;
my $p = (k => $e);
$p.value = 10;
is $e, 10, 'pair value aliases the variable container';

# := bind
my $f = 4;
my $g := $f;
$g = 40;
is $f, 40, ':= binds the two variables to one container';

# slurpy `is raw` aliases each caller argument
sub bump(*@v is raw) { $_++ for @v }
my ($h, $i) = 1, 2;
bump($h, $i);
is $h, 2, 'slurpy is raw writes back to the first source';
is $i, 3, 'slurpy is raw writes back to the second source';

# a variable argument binds into a |c slurpy as its *value*, not as a wrapper
sub cap(|c) { c.list.join('-') }
my $j = 8; my $k = 9;
is cap($j, $k), '8-9', 'a variable argument binds into |c as its value';

# multi dispatch sees through the wrapper to the value's type
multi m(Int $x) { 'int' }
multi m(Str $x) { 'str' }
my $n = 3; my $s = 'x';
is m($n), 'int', 'multi dispatch on an Int variable argument';
is m($s), 'str', 'multi dispatch on a Str variable argument';

# .VAR reaches the variable behind the wrapper
my $v = 5;
is $v.VAR.name, '$v', '.VAR.name of a variable';

# the wrapper is transparent to type introspection
sub what-of($x) { $x.WHAT.gist }
my $w = 42;
is what-of($w), '(Int)', 'a variable argument introspects as its value type';
