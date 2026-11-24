use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 12;

# A positional store into a literal stores through `AT-POS`, which is the
# literal itself at index 0: it dies with X::Assignment::RO naming the value,
# as on rakudo (#18960).
throws-like { 5[0] = 3 }, X::Assignment::RO, message => 'Cannot modify an immutable Int (5)',
    'an Int literal';
throws-like { "a"[0] = 3 }, X::Assignment::RO, message => 'Cannot modify an immutable Str (a)',
    'a Str literal';
throws-like { 5.5[0] = 3 }, X::Assignment::RO, message => 'Cannot modify an immutable Rat (5.5)',
    'a Rat literal';
throws-like { True[0] = 3 }, X::Assignment::RO, message => 'Cannot modify an immutable Bool (True)',
    'a Bool literal';
throws-like { (5)[0] = 3 }, X::Assignment::RO, message => 'Cannot modify an immutable Int (5)',
    'a parenthesized literal';

try { 5[0] = 3 }
is $!.value, 5, 'the exception carries the refused value';
is $!.typename, 'Int', 'and its type name';

# Any other index dies as the read does; the right-hand side runs first.
is_run '5[1] = say "rhs"', { :out("rhs\n"), :err(/'Index out of range. Is: 1, should be in 0..0'/) },
    'an index past the end dies as the read does, after the right-hand side';
is_run '5[0] = say "rhs"', { :out("rhs\n"), :err(/'Cannot modify an immutable Int (5)'/) },
    'the right-hand side runs before the store dies';

# A container is still assignable.
my $x = 5;
$x[0] = 3;
is $x, 3, 'a scalar variable holding the value is assigned';
sub f is rw { my $ = 5 }
lives-ok { f()[0] = 3 }, 'an is rw routine result is assignable';

# A bare literal assignment reports its value too.
try { EVAL q[120 = 3] }
is $!.value, 120, 'a bare literal assignment carries the refused value';
