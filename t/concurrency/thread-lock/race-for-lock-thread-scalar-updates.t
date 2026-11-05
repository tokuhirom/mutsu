use Test;

# A scalar the body of a `race for` / `hyper for` names is one binding shared by
# every batch thread: updates made under a Lock must all survive.

plan 6;

my $n = 0;
my $l = Lock.new;
race for ^8 { $l.protect: { $n++ } }
is $n, 8, 'a Lock.protect closure increments the shared scalar';

my $m = 0;
race for ^8 { $l.protect: { $m = $m + 1 } }
is $m, 8, 'a Lock.protect closure assigns the shared scalar';

my $k = 0;
race for ^8 { $l.lock; $k++; $l.unlock }
is $k, 8, 'lock/unlock around a direct increment';

my $h = 0;
hyper for ^8 { $l.protect: { $h++ } }
is $h, 8, 'hyper for shares the scalar too';

my @r;
race for ^16 -> $i {
    my $before = $i;
    sleep 0.001 * (16 - $i) / 16;
    $l.protect: { @r.push("$before:$i") if $before != $i };
}
is @r.elems, 0, 'a per-iteration lexical keeps its own value';

my $t = 0;
race for ^8 { my $v = $_; sleep 0.001; $l.protect: { $t++ if $v != $_ } }
is $t, 0, 'the topic stays per iteration';
