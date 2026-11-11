use Test;
# ADR-15519 unit 3: Promise.get-await-handle, Channel.get-await-handle and
# Supply.get-await-handle return real Awaitable::Handle objects (identical
# under rakudo).
plan 28;

my $kept = Promise.kept(5).get-await-handle;
ok $kept ~~ Awaitable::Handle, 'kept promise: handle does the role';
ok $kept.already && $kept.success, 'kept promise: already, success';
is $kept.result, 5, 'kept promise: result';

my $broken = Promise.broken("x").get-await-handle;
ok $broken ~~ Awaitable::Handle, 'broken promise: handle does the role';
ok $broken.already && !$broken.success, 'broken promise: already, not success';
is $broken.cause.message, 'x', 'broken promise: cause';

my $p = Promise.new;
my $pending = $p.get-await-handle;
ok $pending ~~ Awaitable::Handle, 'pending promise: handle does the role';
nok $pending.already, 'pending promise: not already';
my $got;
$pending.subscribe-awaiter(-> $ok, $r { $got = ($ok, $r) });
$p.keep(7);
for ^100 { last if $got; sleep 0.02 }
is-deeply $got, (True, 7), 'pending promise: subscriber gets success + result';

my $q = Promise.new;
my $pq = $q.get-await-handle;
my $got2;
$pq.subscribe-awaiter(-> $ok, $r { $got2 = ($ok, $r.message) });
$q.break("boom");
for ^100 { last if $got2; sleep 0.02 }
is-deeply $got2, (False, 'boom'), 'pending promise: subscriber gets failure + cause';

my $c = Channel.new; $c.send(3); $c.close;
my $hc = $c.get-await-handle;
ok $hc ~~ Awaitable::Handle, 'queued channel: handle does the role';
ok $hc.already && $hc.success, 'queued channel: already, success';
is $hc.result, 3, 'queued channel: received value';

my $closed = Channel.new; $closed.close;
my $hcl = $closed.get-await-handle;
ok $hcl.already && !$hcl.success, 'closed empty channel: already failure';
is $hcl.cause.^name, 'X::Channel::ReceiveOnClosed', 'closed empty channel: cause type';

my $open = Channel.new;
my $ho = $open.get-await-handle;
ok $ho ~~ Awaitable::Handle, 'open empty channel: handle does the role';
nok $ho.already, 'open empty channel: not already';
my $got3;
$ho.subscribe-awaiter(-> $ok, $r { $got3 = ($ok, $r) });
$open.send(11);
for ^100 { last if $got3; sleep 0.02 }
is-deeply $got3, (True, 11), 'open empty channel: subscriber gets the sent value';

my $p2 = Promise.new; my $c2 = Channel.new;
start { sleep 0.05; $p2.keep(9); $c2.send(4) };
is (await $p2), 9, 'await on a native Promise still works';
is (await $c2), 4, 'await on a native Channel still works';
is (await Promise.kept(1), Promise.kept(2)).join(','), '1,2', 'await on a list of promises';

# A supply's handle is never settled up front; subscribing taps the supply and
# settles with its last value when done, or with the exception it quits with.
my $hs = Supply.from-list(7, 8).get-await-handle;
ok $hs ~~ Awaitable::Handle, 'supply: handle does the role';
nok $hs.already, 'supply: not already';
my $got4;
$hs.subscribe-awaiter(-> $ok, $r { $got4 = ($ok, $r) });
for ^100 { last if $got4; sleep 0.02 }
is-deeply $got4, (True, 8), 'supply: subscriber gets the last value';

my $got5;
Supply.from-list().get-await-handle.subscribe-awaiter(-> $ok, $r { $got5 = ($ok, $r) });
for ^100 { last if $got5; sleep 0.02 }
is-deeply $got5, (True, Nil), 'empty supply: subscriber gets Nil';

my $sq = Supplier.new;
my $hq = $sq.Supply.get-await-handle;
my $got6;
$hq.subscribe-awaiter(-> $ok, $r { $got6 = ($ok, $r.message) });
$sq.emit(1);
$sq.quit(X::AdHoc.new(payload => 'bad'));
for ^100 { last if $got6; sleep 0.02 }
is-deeply $got6, (False, 'bad'), 'quitting supply: subscriber gets failure + cause';

my $sl = Supplier.new;
my $hl = $sl.Supply.get-await-handle;
my $got7;
$hl.subscribe-awaiter(-> $ok, $r { $got7 = ($ok, $r) });
$sl.emit(5); $sl.emit(6); $sl.done;
for ^100 { last if $got7; sleep 0.02 }
is-deeply $got7, (True, 6), 'live supply: subscriber gets the last value emitted before done';
is (await Supply.from-list(1, 2, 3)), 3, 'await on a Supply still works';
