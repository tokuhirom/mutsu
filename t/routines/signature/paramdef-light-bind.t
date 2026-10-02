use v6;
use Test;

# A WhateverCode (`* + *`) and a multi-parameter pointy block (`-> $a, $b`)
# carry ParamDefs, so unlike `-> $a { }` (t/routines/closure/pointy-block-light-bind.t) they used
# to reach the general signature binder's real path on every call. #10702 gave
# the plain subset of that shape -- untyped positional `$` parameters, at most
# `is raw` -- a light bind with its per-parameter answers settled once per code
# object. These tests pin the behaviour it has to reproduce exactly, and the
# signatures and arguments it must decline so the general binder still runs.

plan 30;

# --- the basic bind ---------------------------------------------------------

is (* + *)(40, 2), 42, 'a two-parameter WhateverCode binds both arguments';
is (* ~ * ~ *)('a', 'b', 'c'), 'abc', '...and a three-parameter one';
is (-> $a, $b { "$a|$b" })(1, 2), '1|2', 'a two-parameter pointy block binds in order';
is (1, 2, * + * ... *)[^8].join(','), '1,2,3,5,8,13,21,34',
    'a sequence generator calls its WhateverCode once per element';

# --- itemization ------------------------------------------------------------

my @arr = 1, 2, 3;
is (-> $a, $b { $a.raku })(@arr, 0), '$[1, 2, 3]',
    'a plain `$` parameter itemizes an array argument';
is (-> $a, $b { my @l = $a; @l.elems })(@arr, 0), 1,
    '...so it is one item in list context';
is (-> $a is raw, $b { $a.raku })([1, 2], 0), '[1, 2]',
    'an `is raw` parameter keeps the argument as passed';
my %h = a => 1;
is (-> $a, $b { $a.raku })(%h, 0), '${:a(1)}', 'a hash argument is itemized too';

# --- readonly ---------------------------------------------------------------

throws-like { (-> $a, $b { $a = 3 })(1, 2) }, Exception,
    'a plain parameter stays readonly';
throws-like { (-> $a is raw, $b { $a = 3 })(1, 2) }, Exception,
    'an `is raw` parameter bound to a value is not writable either';

# --- an `is raw` parameter bound to a variable aliases it (declined) ---------

my $target = 1;
(-> $a is raw, $b { $a = $b })($target, 7);
is $target, 7, 'an `is raw` parameter writes through to the caller variable';

# --- a Callable argument ----------------------------------------------------

is (-> $f, $x { $f($x) })(-> $n { $n * 3 }, 7), 21, 'a Callable argument is invocable';
is (-> $f, $x { $f($x) })(&sqrt, 49), 7, '...including a routine passed by reference';

# --- arity and named arguments (declined) -----------------------------------

throws-like { (* + *)(1) }, Exception, message => /'Too few positionals'/,
    'a short call reports "Too few positionals"';
throws-like { (* + *)(1, 2, 3) }, Exception, message => /'Too many positionals'/,
    'a surplus call reports "Too many positionals"';
throws-like { (-> $a, $b { $a })(1, 2, :x(3)) }, Exception,
    message => /'Unexpected named argument'/,
    'a named argument is still rejected';

# --- signatures the light bind refuses --------------------------------------

throws-like { (-> Int $a, $b { $a })('x', 1) }, X::TypeCheck::Binding::Parameter,
    'a typed parameter is still type-checked';
is (-> $a, $b = 5 { $a + $b })(1), 6, 'a default is still applied';
is (-> $a, $b where * > 0 { $b })(1, 2), 2, 'a `where` clause still accepts';
throws-like { (-> $a, $b where * > 0 { $b })(1, -2) }, Exception,
    '...and still rejects';
is (-> $a is copy, $b { $a += $b; $a })(1, 2), 3, 'an `is copy` parameter is writable';
is (-> $a, *@rest { @rest.elems })(1, 2, 3), 2, 'a slurpy still collects';

# --- captures ---------------------------------------------------------------

my $n = 5;
my $add = -> $a, $b { $a + $b + $n };
is $add(1, 2), 8, 'a free variable is visible to a light-bound body';
$n = 10;
is $add(1, 2), 13, '...and tracks the caller-side mutation';

is (-> $a, $b { -> { $a + $b } })(3, 4)(), 7,
    'a nested closure captures the light-bound parameters';

my $a = 'caller';
my $b = 'caller';
(-> $a, $b { $a ~ $b })(1, 2);
is "$a $b", 'caller caller', 'the parameters do not leak into same-named caller lexicals';

# --- the native list loops --------------------------------------------------

is (3, 1, 2).sort(-> $x, $y { $y <=> $x }).join(','), '3,2,1',
    '.sort with a two-parameter comparator';
is (1, 2, 3, 4).map(-> $x, $y { $x * $y }).join(','), '2,12',
    '.map with a two-parameter block';
is ([+] (1..4).map(* * 2)), 20, 'a one-parameter WhateverCode is unaffected';

# --- recursion --------------------------------------------------------------

my $gcd;
$gcd = -> $x, $y { $y == 0 ?? $x !! $gcd($y, $x % $y) };
is $gcd(48, 18), 6, 'a self-recursive two-parameter block binds each frame separately';
