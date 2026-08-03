use v6;
use Test;

# A closure's caller-writeback decides which captured free variables to
# propagate by comparing each one's value at entry with its value at exit. It
# used plain `!=` -- but `Value`'s equality is Raku's SEMANTIC equality, and a
# `does` mixin is equal to the value it wraps. So `$x does R` inside a block
# reported `entry=Hash now=Mixin` as *unchanged*, the name landed in
# `unchanged_free`, and the writeback skipped it: the caller kept the un-mixed
# value.
#
# `lives-ok { $a does role { ... } }` is the shape that hides this -- the block
# runs through a routine, so the writeback is what has to carry the mixin back.

plan 9;

role R { has $.tag = "tagged" }
role Parametrized[$v] { has $.attr = $v }

sub call1(&b) { b() }

# The shape roast/S14-roles/anonymous.t and S14-roles/parameterized-mixin.t use.
my $a = {:x};
call1 { $a does R };
is $a.tag, 'tagged', 'a `does` inside a block invoked by a routine reaches the caller';

my $p = 0;
call1 { $p does Parametrized[42] };
is $p.attr, 42, '...including a parameterized role';

# Test's own `lives-ok`, which is the real call shape.
my $l = {:x};
lives-ok { $l does R }, 'the mixin lives';
is $l.tag, 'tagged', '...and is visible after lives-ok returns';

# The shapes that already worked must keep working.
my $bare = {:x};
{ $bare does R }
is $bare.tag, 'tagged', 'a `does` in a bare block still reaches the enclosing scope';

my $top = {:x};
$top does R;
is $top.tag, 'tagged', 'a `does` at file scope still works';

# A same-valued reassignment inside a block must not be mistaken for a change in
# the other direction either: the writeback is allowed to run, it just has to be
# correct.
my $same = 5;
call1 { $same = 5 };
is $same, 5, 'a same-valued write is harmless';

# An ordinary change still propagates.
my $n = 1;
call1 { $n = 2 };
is $n, 2, 'an ordinary change still propagates';

# And a read-only capture is not clobbered.
my $ro = 'keep';
call1 { my $unused = $ro };
is $ro, 'keep', 'a read-only capture is untouched';
