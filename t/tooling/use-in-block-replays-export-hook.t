use v6;
use lib $*PROGRAM.parent(2).add('lib-tail-block-scope');
use Test;

plan 8;

# A block-scoped `use` of a module whose import goes through a `sub EXPORT`
# hook runs on every call of the routine. Rakudo runs the hook once, at
# compile time, so every call sees the values of that one run (#19102).

sub f($v) {
    use HookReplay;
    (hooked($v), hook-term, $hook-var);
}

is-deeply f("one"), ("hooked:one:1", "term:1", 42), 'first call';
is-deeply f("two"), ("hooked:two:1", "term:1", 42), 'second call sees the same hook run';
is-deeply f("three"), ("hooked:three:1", "term:1", 42), 'third call too';
sub k() {
    use HookReplay;
    hook-term;
}
my $k = k();
is k(), $k, 'another use site keeps its own run on every call';

# A routine written outside the block does not see the hook's names.
sub outside() { (try EVAL 'hook-term') // 'unseen' }
sub calls-outside() {
    use HookReplay;
    outside();
}
is calls-outside(), 'unseen', 'a callee outside the block cannot see the term';
is calls-outside(), 'unseen', '... nor on a later call';

is $PROCESS::hook-replay-runs, 3, 'the hook ran once per use site, not once per call';

dies-ok { EVAL 'hooked("x")' }, 'the routine is not visible outside the blocks';
