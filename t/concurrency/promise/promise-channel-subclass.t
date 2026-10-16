use Test;

# #13634: an instance of a user subclass of `Promise` or `Channel` is a promise
# or a channel: its built-in methods (the rows of ADR-11276 §9.118) answer on
# the native value it carries, so `await`, `.then` and `.vow` work on it.
# Answers checked against Rakudo 2026.09.

plan 15;

class P is Promise {
    has $.tag = 't';
    method hi { 'hi ' ~ self.status }
}

my $p = P.new;
is $p.hi, 'hi Planned', 'a user method sees the promise';
is $p.tag, 't', 'a declared attribute';
$p.keep(3);
is $p.result, 3, '.result';
is $p.status, Kept, '.status';
is await($p), 3, 'await takes the subclass instance';
isa-ok $p, Promise, 'it is a Promise';

my $q = P.new;
my $t = $q.then({ .result * 2 });
$q.keep(5);
is $t.result, 10, '.then on a subclass instance';

my $r = P.new;
my $v = $r.vow;
$v.break('no');
is $r.cause.message, 'no', '.vow and .cause';
is P.start({ 7 }).result, 7, 'P.start';

class C is Channel { method extra { 1 } }
my $c = C.new;
$c.send($_) for 1..3;
is $c.receive, 1, 'Channel subclass .receive';
is $c.poll, 2, '.poll';
$c.close;
is $c.List, (3,), '.List drains the channel it carries';
is $c.extra, 1, 'a user method';
ok $c.closed.Bool, '.closed';
throws-like { $c.receive }, X::Channel::ReceiveOnClosed, 'receive on the closed channel';
