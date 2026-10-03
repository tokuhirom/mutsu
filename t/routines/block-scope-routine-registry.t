use Test;

# A block snapshots the routine registry on entry and puts it back on exit
# (so a `my sub` / `my token` declared inside stops being visible). The
# snapshot is a set of copy-on-write tables, and an exit from a block that
# changed none of them is a no-op (#9170). These pin that both halves still
# scope correctly.

plan 12;

sub outer-sub { 'outer' }

# A block that declares nothing leaves every routine resolvable.
my $t = 0;
for ^50 { { my $y = 1; $t += $y } }
is $t, 50, 'a bare block that declares nothing runs every iteration';
is outer-sub(), 'outer', 'a routine declared outside stays callable';

# A block-local `my sub` is gone after the block.
{
    my sub inner { 'inner' }
    is inner(), 'inner', 'a my sub is callable inside its block';
}
throws-like { EVAL 'inner()' }, X::Undeclared::Symbols,
    'the my sub is gone after its block';

# The same, repeated: the restore after the first exit must not hide a later
# declaration of the same block.
my @seen;
for ^3 -> $i {
    {
        my sub each-time { 'each' ~ $i }
        @seen.push: each-time();
    }
}
is @seen.join(','), 'each0,each1,each2', 'a my sub re-declared per iteration';

# A block-local `my sub` shadowing an outer one is undone on exit.
{
    my sub outer-sub { 'shadow' }
    is outer-sub(), 'shadow', 'the inner my sub shadows inside the block';
}
is outer-sub(), 'outer', 'the outer routine is back after the block';

# A block-local token is scoped to the block.
{
    my token digit-pair { \d \d }
    ok '42' ~~ / <digit-pair> /, 'a my token matches inside its block';
}
dies-ok { EVAL q['42' ~~ / <digit-pair> /] }, 'the my token is gone after its block';

# A grammar declared in a block keeps its tokens.
{
    grammar BlockG { token TOP { <num> }; token num { \d+ } }
}
ok BlockG.parse('123'), 'a grammar declared in a block keeps its tokens';

# Method dispatch is unaffected by blocks that change nothing.
class Counter { has $.n = 0; method bump { $!n++ } }
my $c = Counter.new;
for ^20 { { my $z = 1 }; $c.bump }
is $c.n, 20, 'method calls interleaved with empty-effect blocks';

# An `our sub` declared in a block survives the block.
{
    our sub survives { 'ours' }
}
is OUR::<&survives>(), 'ours', 'an our sub declared in a block survives it';
