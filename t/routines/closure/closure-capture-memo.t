use Test;

# The closure-capture env is memoized across repeated creations of the same
# closure literal from an unchanged scope (`src/vm/vm_capture_cache.rs`). These
# pin the observable semantics that memo must not change: a reused capture is
# only ever legal while the visible env is byte-for-byte the one it was built
# from, and everything that varies per creation still has to be re-applied.

plan 8;

# A free variable is re-read from the creating frame's live slot on every
# creation, so a closure literal in a loop must capture that iteration's value
# even once the memo is armed (creation 3 onwards).
my @per-iteration;
for ^5 -> $i {
    @per-iteration.push({ $i * 10 });
}
is(
    @per-iteration.map({ $_.() }).join(','),
    '0,10,20,30,40',
    'a loop-created closure keeps its own iteration value once the memo is armed',
);

# The same shape for a WhateverCode, which additionally writes its own
# closure-identity marker into the captured env after the memo hands it back.
my @whatevers;
for ^4 -> $n {
    @whatevers.push($n + *);
}
is(
    @whatevers.map({ $_.(1) }).join(','),
    '1,2,3,4',
    'a WhateverCode created in a loop is not confused by a reused capture',
);

# A dynamic variable lives in the env, so changing one between two creations of
# the same literal has to invalidate the memo.
my $*topicish = 'first';
my $before = { $*topicish };
$*topicish = 'second';
my $after = { $*topicish };
is($after.(), 'second', 'a closure created after a dynamic changed sees the new value');
is($before.(), 'second', 'a dynamic is read at call time, not frozen at creation');

# Declaring a new lexical between two creations changes the visible env too.
my $seen-before = { $^a + 1 };
my $mid = 41;
my $seen-after = { $mid + $^a };
is($seen-before.(1), 2, 'a closure created before a later declaration still runs');
is($seen-after.(1), 42, 'a closure created after it captures the new lexical');

# `self` is materialized into the capture from the creating frame, so two
# instances calling the same method must not share one memoized capture.
class Counter {
    has $.tag;
    method make-closure() { return { $.tag } }
}
my $a-closure = Counter.new(tag => 'a').make-closure;
my $b-closure = Counter.new(tag => 'b').make-closure;
is($a-closure.(), 'a', 'the first instance closure keeps its own invocant');
is($b-closure.(), 'b', 'the second instance closure does not inherit the first');
