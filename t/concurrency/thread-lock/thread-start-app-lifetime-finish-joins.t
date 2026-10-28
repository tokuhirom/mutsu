use Test;

# #15208 (Selkie::Trace's writer thread): `Thread.start(:app_lifetime, ...)`
# is not waited for at program exit, but `.finish` on that very thread joins it
# like any other. mutsu dropped the join handle, so `.finish` returned at once
# and the writer's output was lost.

plan 3;

my @seen;
my $ch = Channel.new;
my $t = Thread.start(:app_lifetime, name => 'writer', sub {
    for $ch.list -> $e { @seen.push($e) }
});
$ch.send(1);
$ch.send(2);
$ch.close;
$t.finish;
is-deeply @seen, [1, 2], '.finish waits for an :app_lifetime thread';

my @plain;
my $c2 = Channel.new;
my $t2 = Thread.start(sub { for $c2.list -> $e { @plain.push($e) } });
$c2.send(3);
$c2.close;
$t2.finish;
is-deeply @plain, [3], 'a plain thread as before';

my $ran = 0;
Thread.start(:app_lifetime, sub { sleep 0.3; $ran = 1 }).finish;
is $ran, 1, 'the thread body has run once finish returns';
