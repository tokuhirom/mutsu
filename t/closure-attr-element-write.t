use v6;
use Test;

plan 16;

# Element-level mutation of array/hash attributes through a returned closure
# must accumulate in the instance (cell-direct), not clobber earlier writes
# via a stale captured env copy.

class Cache {
    has %!h;
    has @!a;

    method h-writer() { -> $k, $v { %!h{$k} = $v } }
    method a-writer() { -> $i, $v { @!a[$i] = $v } }
    method a-pusher() { -> $v { @!a.push($v) } }
    method set($k, $v) { %!h{$k} = $v }
    method del($k) { %!h{$k}:delete }
    method get($k) { %!h{$k} }
    method h() { %!h }
    method a() { @!a.List }
}

# closure hash element writes accumulate (first write must survive)
my $c = Cache.new;
my $w = $c.h-writer;
$w('a', 1);
$w('b', 2);
$w('c', 3);
is $c.get('a'), 1, 'first closure hash element write survives';
is $c.get('b'), 2, 'second closure hash element write survives';
is $c.get('c'), 3, 'third closure hash element write survives';
is $c.h.elems, 3, 'all closure hash writes accumulated';

# interleaved method and closure writes see each other
my $c2 = Cache.new;
my $w2 = $c2.h-writer;
$w2('x', 10);
$c2.set('y', 20);
$w2('z', 30);
is $c2.h.elems, 3, 'closure and method hash writes interleave';
is $c2.get('y'), 20, 'method write between closure writes survives';

# closure array element writes accumulate
my $c3 = Cache.new;
my $aw = $c3.a-writer;
$aw(0, 'first');
$aw(2, 'third');
is $c3.a[0], 'first', 'first closure array element write survives';
is $c3.a[2], 'third', 'second closure array element write survives';

# closure push accumulates
my $c4 = Cache.new;
my $p = $c4.a-pusher;
$p(1);
$p(2);
$p(3);
is $c4.a.elems, 3, 'closure pushes accumulate';
is $c4.a.join(','), '1,2,3', 'closure pushes in order';

# :delete on a hash attribute reaches the instance
my $c5 = Cache.new;
$c5.set('keep', 1);
$c5.set('drop', 2);
$c5.del('drop');
is $c5.h.elems, 1, ':delete on hash attribute removes the key';
is $c5.get('keep'), 1, ':delete leaves other keys';

# :delete after closure writes
my $c6 = Cache.new;
my $w6 = $c6.h-writer;
$w6('a', 1);
$w6('b', 2);
$c6.del('a');
is $c6.h.elems, 1, ':delete works after closure writes';
is $c6.get('b'), 2, 'closure write survives :delete of another key';

# two instances have independent attribute storage
my $i1 = Cache.new;
my $i2 = Cache.new;
my $w-i1 = $i1.h-writer;
my $w-i2 = $i2.h-writer;
$w-i1('k', 'one');
$w-i2('k', 'two');
is $i1.get('k'), 'one', 'instance 1 closure writes its own cell';
is $i2.get('k'), 'two', 'instance 2 closure writes its own cell';
