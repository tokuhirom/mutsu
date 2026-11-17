use Test;

# `our $x` installs a lexical alias of the package variable. A sub that
# rebinds the name with `:=` rebinds that alias, which the declaring scope
# and every other sub see after the call returns; the package variable keeps
# its container (#17508). A plain assignment through the alias, or through
# `$OUR::x`, still reaches both. Every expectation was checked against rakudo.

plan 16;

our %g; our @a; our $s;
sub bind-hash($x) { %g := $x }
sub bind-array($x) { @a := $x }
sub bind-scalar($x) { $s := $x }
sub read-all() { "{%g.keys} {@a} $s" }

bind-hash({a => 1});
is-deeply %g, {a => 1}, 'a hash `our` rebound in a sub keeps the new binding';
bind-array([1]);
is-deeply @a, [1], 'an array `our` rebound in a sub keeps the new binding';
my $v = 5;
bind-scalar($v);
is $s, 5, 'a scalar `our` rebound in a sub keeps the new binding';
is read-all(), 'a 1 5', 'another sub reads the rebound aliases';
is-deeply %OUR::g, {}, 'the package hash keeps its own container';
is-deeply @OUR::a, [], 'the package array keeps its own container';
nok $OUR::s.defined, 'the package scalar keeps its own container';

my %m;
sub bind-my($x) { %m := $x }
bind-my({q => 1});
is-deeply %m, {q => 1}, 'a `my` rebound in a sub (unchanged)';

our $t = 1;
our @l = 1;
sub write-t() { $t = 2; @l.push: 2 }
sub read-t() { "$t @l[]" }
write-t();
is $t, 2, 'an assignment in a sub reaches the alias';
is $OUR::t, 2, 'an assignment in a sub reaches the package variable';
is-deeply @OUR::l, [1, 2], 'a push in a sub reaches the package array';
$OUR::t = 7;
is $t, 7, '`$OUR::t = ...` reaches the alias';
is read-t(), '7 1 2', '`$OUR::t = ...` reaches a sub that reads the alias';
$GLOBAL::t = 8;
is $t, 8, '`$GLOBAL::t = ...` reaches the alias';
is read-t(), '8 1 2', '`$GLOBAL::t = ...` reaches a sub that reads the alias';
$t = 3;
is $OUR::t, 3, 'an assignment to the alias reaches the package variable';
