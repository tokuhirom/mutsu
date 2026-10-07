use v6;
use Test;

# Semaphore's and Thread's methods are rows of the built-in method table,
# reached through their owner.

plan 16;

my $s = Semaphore.new(2);
ok $s.try_acquire, 'try_acquire takes a permit';
$s.acquire;
nok $s.try_acquire, 'no permit is left';
$s.release;
ok $s.try_acquire, 'release gave one back';
ok Semaphore.^can('acquire') && Semaphore.^can('try_acquire') && Semaphore.^can('release'),
    '.^can sees the rows';

my $r = 0;
my $sem = Semaphore.new(1);
await (^4).map: { start { for ^50 { $sem.acquire; $r++; $sem.release } } };
is $r, 200, 'a semaphore serializes the increments';

my $t = Thread.new(code => { 1 }, name => "worker");
is $t.name, 'worker', 'name';
nok $t.app_lifetime, 'app_lifetime defaults to False';
nok $t.is-initial-thread, 'a new thread is not the initial one';
ok $t.id > 0, 'id';
is $t.Numeric, $t.id, 'Numeric is the id';
is $t.Str.subst(/\d+/, 'N'), 'Thread<N>(worker)', 'Str';
is $t.gist.subst(/\d+/, 'N'), 'Immortal Thread #N (worker)', 'gist, as Rakudo spells it';
is Thread.new(code => { 1 }, name => 'w', :app_lifetime).gist.subst(/\d+/, 'N'), 'Thread #N (w)', 'gist of an app_lifetime thread';
is Thread.new(code => { 1 }).gist.subst(/\d+/, 'N'), 'Immortal Thread #N', 'gist of an anonymous thread';
$t.run;
ok $t.finish, 'finish waits for the thread';
ok Thread.^can('finish') && Thread.^can('id'), '.^can sees the rows';
