use Test;
use nqp;

# #17723 (ParaSeq `.map` at degree > 1 hanging): `nqp::eqaddr($next, $!attr)`
# in a private method asks whether `$!attr` was `:=`-bound. That check ran
# inside `with_self_attr`, which already held the instance's attribute read
# lock, and then locked the same cell a second time. With another thread's
# attribute `nqp::atomicinc_i` queued for the write lock in between, the
# writer-preferring RwLock blocked the second read behind the writer while the
# writer waited on the first: a deadlock (every run, before the fix).

plan 2;

class Shared {
    has atomicint $!n;
    has $!a;
    method bump { nqp::atomicinc_i($!n) }
    method !same($next is raw) { nqp::eqaddr($next, $!a) }
    method same { my $x := 1; self!same($x) }
    method n { nqp::atomicload_i($!n) }
}

my $s = Shared.new;
my $deadline = now + 2;
my atomicint $bumps;
my @workers = flat
    (^2).map({ start { while now < $deadline { $s.bump; $bumps⚛++ } } }),
    (^2).map({ start { while now < $deadline { $s.same } } });

await Promise.anyof(Promise.allof(@workers), Promise.in(60));
ok @workers.map(*.status).all ~~ Kept, 'readers and atomic writers of one instance all finish';
is $s.n, $bumps, 'every atomic increment is counted';
