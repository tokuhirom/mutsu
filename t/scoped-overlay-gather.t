use v6;
use Test;

# Regression pin for the scoped/overlay env on the gather/lazy-list VM paths
# (force_lazy_list_vm / force_lazy_list_vm_n) and the closure dispatch path
# (call_compiled_closure). See docs/vm-dual-store.md (Slice 6). The gather body
# runs under a scoped overlay whose parent is the gather's captured env; its
# writes land in the overlay and side effects on captured/outer vars propagate.

plan 10;

# --- basic gather ---
my @g = gather { take $_ * 2 for 1..5 };
is @g.List, (2, 4, 6, 8, 10), 'basic gather/take';

# --- gather body side effect on a captured outer var persists ---
my $count = 0;
my @h = gather { for 1..3 { $count++; take $count } };
is @h.List, (1, 2, 3), 'gather body sees mutated captured var';
is $count, 3, 'captured var mutation persists after gather force';

# --- lazy gather + slice forces the coroutine path ---
my @lazy = lazy gather { my $i = 0; loop { take $i++ } };
is @lazy[^5].List, (0, 1, 2, 3, 4), 'lazy gather coroutine first slice';
is @lazy[^3].List, (0, 1, 2), 'lazy gather coroutine cached re-slice';

# --- coroutine state (a captured counter) persists across take batches ---
my $c = 0;
my @seq = lazy gather { loop { $c++; take $c } };
is @seq[^4].List, (1, 2, 3, 4), 'coroutine batch 1';
is @seq[^5].List, (1, 2, 3, 4, 5), 'coroutine batch 2 (state persisted)';

# --- closures (call_compiled_closure under scoped overlay) ---
my @doubled = (1, 2, 3, 4).map(* * 2);
is @doubled.List, (2, 4, 6, 8), 'map block closure';

my $acc = 0;
(1, 2, 3).map({ $acc += $_ });
is $acc, 6, 'closure mutates captured outer var';

# --- closure factory: independent per-instance captured state ---
sub make-counter() { my $n = 0; return { $n++ } }
my &a = make-counter();
my &b = make-counter();
a(); a(); b();
is (a(), b()).List, (2, 1), 'independent closure instances keep own state';
