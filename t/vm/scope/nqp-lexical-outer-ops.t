use v6;
use nqp;
use Test;

# nqp::getlexouter / getlexcaller / bindlexdyn / lexprimspec / getlexref_*:
# lexicals named by a runtime string, looked up outside the running scope or
# through a reference. Expected values checked against Rakudo. Each lexical is
# captured by a closure: Rakudo lowers an uncaptured one away.
plan 23;

my $x = 5;
my &g = { $x };

sub f { nqp::getlexouter(q/$x/) }
is f(), 5, 'getlexouter reads an enclosing lexical';
sub shadowing { my $x = 6; my &k = { $x }; nqp::getlexouter(q/$x/) }
is shadowing(), 5, 'getlexouter skips the running scope\'s own binding';
sub in-block { my $x = 6; my &k = { $x }; my $r; if 1 { $r = nqp::getlexouter(q/$x/) }; $r }
is in-block(), 6, 'getlexouter from a nested block sees the routine\'s binding';
sub own-only { my $q = 1; my &k = { $q }; nqp::getlexouter(q/$q/) }
throws-like { own-only() }, Exception, message => "No lexical found with name '\$q'",
    'getlexouter of a name only the running scope declares dies';
throws-like { nqp::getlexouter(q/$nope/) }, Exception,
    message => "No lexical found with name '\$nope'", 'getlexouter of an unknown name dies';
is nqp::getlexouter(q/&say/).^name, 'Sub', 'getlexouter finds a setting routine';
my $name = q/$x/;
is nqp::getlexouter($name), 5, 'getlexouter with a computed name';

sub c { nqp::getlexcaller(q/$x/) }
sub d { my $x = 42; my &k = { $x }; c() }
is d(), 42, 'getlexcaller reads the caller\'s lexical';
sub e { c() }
is e(), 5, 'getlexcaller walks out through the callers';
ok nqp::isnull(nqp::getlexcaller(q/$nope/)), 'getlexcaller of an unknown name is null';

my $*D = 1;
sub bd { nqp::bindlexdyn('$*D', 8); $*D }
is bd(), 8, 'bindlexdyn binds the caller\'s dynamic, seen by the running frame';
is $*D, 8, 'bindlexdyn binding is seen by the declaring frame';
sub bd-own { my $*D = 2; my &k = { $*D }; nqp::bindlexdyn('$*D', 9); $*D }
is bd-own(), 2, 'bindlexdyn skips the running frame\'s own declaration';
is $*D, 9, '... and binds the caller\'s';
sub inner { nqp::bindlexdyn('$*D', 3) }
sub mid { my $*D = 0; inner(); $*D }
is mid(), 3, 'bindlexdyn binds the nearest caller that declares the dynamic';
throws-like { nqp::bindlexdyn('$*NOPE', 1) }, Exception,
    message => "Dynamic variable '\$*NOPE' not found", 'bindlexdyn of an unknown dynamic dies';

my int $i = 3; my &gi = { $i };
my num $n = 1.5e0; my &gn = { $n };
my str $s = 'a'; my &gs = { $s };
is-deeply (<$i $n $s $x>.map: { nqp::lexprimspec(nqp::ctx(), $_) }).List, (1, 2, 3, 0),
    'lexprimspec: int, num, str, object';
throws-like { nqp::lexprimspec(nqp::ctx(), q/$nope/) }, Exception,
    message => "Frame has no lexical with name '\$nope'", 'lexprimspec of an unknown name dies';

my $ri := nqp::getlexref_i(q/$i/);
$ri = 10;
is gi(), 10, 'a write through getlexref_i reaches the closure';
ok nqp::iscont_i($ri), 'getlexref_i answers an int container';
my $rn := nqp::getlexref_n(q/$n/);
$rn = 2.5e0;
is gn(), 2.5e0, 'getlexref_n';
my $rs := nqp::getlexref_s(q/$s/);
$rs = 'z';
is gs(), 'z', 'getlexref_s';
sub rr { my $r := nqp::getlexref_i(q/$i/); $r = 77 }
rr();
is $i, 77, 'getlexref_i from a routine refers to the enclosing lexical';
