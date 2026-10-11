use Test;

# From HTTP::Server::Middleware::JSON: a module-scope `my $err = sub {...}`
# that a module sub reassigns (`json-error`) must be seen by a closure already
# running on a worker thread. A Sub in a `$` scalar used to be refused the
# shared cell, so the worker read a stale spawn-time snapshot.

plan 3;

module M {
    my $err = sub { 'default' };
    our sub setit($v) { $err = $v }
    our sub mk() { sub () { my $e = $err; $e() } }
    our sub mk-args() { sub ($x, $y) { $err($x, $y) } }
}

my $h = M::mk();
my $t = start { sleep 0.3; $h() };
M::setit(sub { 'custom' });
is await($t), 'custom', 'worker sees a Sub reassigned after the spawn';

M::setit(sub ($a, $b) { "default $a $b" });
my $g = M::mk-args();
my $t2 = start { sleep 0.3; $g(1, 2) };
M::setit(sub ($a, $b) { "custom $a $b" });
is await($t2), 'custom 1 2', 'call form `$err($x, $y)` sees the new Sub';

M::setit(sub { 'again' });
is $h(), 'again', 'main thread still sees the reassignment';
