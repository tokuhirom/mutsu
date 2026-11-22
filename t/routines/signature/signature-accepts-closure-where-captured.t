use Test;
use nqp;

# #18661: `Capture ~~ &closure.signature` runs a `where` clause in the
# closure's own scope -- the variable it captured -- not in the scope of
# whoever smartmatches, and never runs the closure's body.

plan 7;

sub mk { my $lim = 3; sub (Int $x where * > $lim) { 1 } }
my &m = mk();
ok \(5) ~~ &m.signature, 'a capture the captured limit admits';
nok \(2) ~~ &m.signature, 'a capture the captured limit refuses';

my $ran = 0;
sub mk2 { my $lim = 3; sub (Int $x where * > $lim) { $ran++; 1 } }
my &n = mk2();
ok \(5) ~~ &n.signature, 'matches';
is $ran, 0, '...without running the body';

my $outer = 10;
sub q(Int $x where * > $outer) { }
ok (\(11) ~~ &q.signature) && !(\(3) ~~ &q.signature), 'a named sub reads its outer scope';

sub r($a, $b where * > 5 = $a * 2) { }
ok !(\(1) ~~ &r.signature) && (\(3) ~~ &r.signature),
    'a computed default is checked against the clause in the routine scope';

is nqp::objectid(&m.signature), nqp::objectid(&m.signature),
    'repeated reads still answer one Signature object';
