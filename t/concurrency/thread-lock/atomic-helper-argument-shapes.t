use Test;

# Pins the argument shapes the `⚛`-operators and `cas` hand their native helpers
# (`__mutsu_atomic_*` / `__mutsu_cas_*`). #12120 sends those calls straight to the
# handler (`Interpreter::exec_atomic_helper_call_op`) instead of through the
# general `CallFunc` resolution, and the handler must see the same operands and
# leave the caller's slots in the same state. Every expected value here is
# rakudo's.

plan 13;

# A Proxy operand is FETCHed before the helper sees it.
my $p := Proxy.new(FETCH => method () { 5 }, STORE => method ($v) { });
my atomicint $a = 1;
my $seen = cas($a, 1, $p);
is $seen, 1, 'cas returns the seen value';
is $a, 5, 'a Proxy new value is FETCHed';

# An atomic op through an `is rw` chain leaves the caller's slot coherent.
sub inner($v is rw) { $v⚛++; $v⚛++ }
sub outer($w is rw) { inner($w); $w }
my atomicint $b = 10;
is outer($b), 12, 'the value through two is-rw frames';
is $b, 12, 'the caller variable after them';

# The result of each atomic operator form.
my atomicint $c = 5;
my $pre = ++⚛$c;
my $post = $c⚛++;
my $fetch = atomic-fetch($c);
atomic-assign($c, 40);
my $fadd = atomic-fetch-add($c, 2);
is "$pre $post $fetch $fadd $c", "6 6 7 40 42", 'pre/post increment, fetch, assign, fetch-add';

# cas with the block form and the delta shortcut.
my atomicint $d = 1;
cas($d, { $_ + 10 });
cas($d, * + 5);
my $blk = cas($d, -> $v { $v * 2 });
is "$blk $d", "32 32", 'cas block form, delta shortcut and a general block';

# cas on an array element and on a hash element.
my atomicint @arr = 1, 2, 3;
my $old = cas(@arr[1], 2, 20);
is $old, 2, 'cas on an array element returns the seen value';
is @arr.join(','), '1,20,3', 'the array element was swapped';
my %h = k => 1;
cas(%h<k>, { $_ + 1 });
is %h<k>, 2, 'cas block form on a hash element';

# A failing atomic op reports an error.
my $died = False;
try {
    my atomicint $z = 1;
    cas($z, 1);
    CATCH { default { $died = True } }
}
ok $died, 'cas with too few operands dies';

# An atomic op as the argument of an ordinary call.
sub twice($n) { $n * 2 }
my atomicint $e = 3;
is twice($e⚛++), 6, 'a post-increment as a call argument';
is twice(atomic-fetch($e)), 8, 'an atomic fetch as a call argument';

# Concurrent use through the direct path.
my atomicint $counter = 0;
await (^4).map({ start { $counter⚛++ for ^250 } });
is $counter, 1000, 'four threads bumping one atomic';
