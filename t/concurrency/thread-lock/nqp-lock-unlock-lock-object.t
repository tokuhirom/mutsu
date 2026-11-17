use Test;
use nqp;

# `nqp::lock` / `nqp::unlock` take a `Lock`'s mutex directly (the
# AttrX::Mooish `attr-var` idiom): reentrant, exclusive across threads, and
# usable on a `Lock` held in an attribute.

plan 5;

my $l := Lock.new;
nqp::lock($l);
nqp::lock($l);
nqp::unlock($l);
nqp::unlock($l);
pass 'the lock is reentrant for its holder';

my $count = 0;
my @workers = (^4).map: {
    start {
        for ^250 {
            nqp::lock($l);
            my $c = $count;
            $count = $c + 1;
            nqp::unlock($l);
        }
    }
};
await @workers;
is $count, 1000, 'the critical section is exclusive across threads';

class Guarded {
    has $!lock = Lock.new;
    has $.n = 0;
    method bump {
        nqp::lock(nqp::decont($!lock));
        $!n++;
        nqp::unlock(nqp::decont($!lock));
        self
    }
}
is Guarded.new.bump.bump.n, 2, 'a lock held in an attribute';

my $taken = Promise.new;
my $release = Promise.new;
nqp::lock($l);
my $other = start {
    $taken.keep;
    nqp::lock($l);
    nqp::unlock($l);
    'got it'
};
await $taken;
sleep 0.1;
nok $other.status ~~ Kept, 'another thread waits while the lock is held';
nqp::unlock($l);
is (await $other), 'got it', 'and takes it once released';
