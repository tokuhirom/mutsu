use Test;

# A closure captures the names its signature's `where` clauses and defaults
# read, so it still sees them after the scope that created it is gone
# (#18570).

plan 5;

sub mk() { my &good = { True }; sub ($x where &good) { "bound $x" } }
is mk()(42), 'bound 42', 'a `where &good` of an escaping sub';

sub mk-block() { my &good = { $_ > 0 }; -> $x where &good { "got $x" } }
is mk-block()(3), 'got 3', 'a `where &good` of an escaping pointy block';
dies-ok { mk-block()(-3) }, 'the captured &good still rejects';

sub mk-default() { my &fb = { 99 }; sub ($x = fb()) { $x } }
is mk-default()(), 99, 'a default that calls an outer &fb';

my $s = do { my &good = { say-nothing(); True }; sub say-nothing { }; sub ($x where &good) { 'ok' } };
is $s(1), 'ok', 'a `where &good` of a sub made in a do block';
