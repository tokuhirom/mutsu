use Test;

# ADR-18637: a captured array shared between threads is read lock-free from a
# snapshot its cell publishes, validated against a version every in-place
# write bumps. These races check that a reader never sees a value that was
# not stored, and that every write is visible once it is synchronized, while
# writers mutate the array through other paths: an element store through a
# routine parameter (an in-place write that does not go through the cell), a
# push that grows the node. (A whole-array assignment racing the readers is
# #18651: its readers can see a value never stored, on the locked path too.)

plan 7;

sub store-into(@a, $i, $v) { @a[$i] = $v }
sub push-onto(@a, $v) { @a.push: $v }

# Read-only sharing: every worker sums the same elements.
{
    my @data = ^64;
    my @shared := @data;
    my @sums = await (^4).map: { start {
        my $s = 0;
        for ^20_000 -> $i { $s += @shared[$i +& 63] }
        $s
    } };
    is-deeply @sums, [629488 xx 4], 'workers reading a shared array agree';
}

# In-place element stores through a parameter race the readers.
{
    my @data = 0 xx 16;
    my @shared := @data;
    my $writer = start {
        for 1..2_000 -> $round { store-into(@shared, $round +& 15, $round) }
        True
    };
    my @ok = await (^3).map: { start {
        my $ok = True;
        for ^20_000 -> $i {
            my $v = @shared[$i +& 15];
            $ok = False unless $v ~~ Int && 0 <= $v <= 2_000 && ($v == 0 || $v +& 15 == $i +& 15);
        }
        $ok
    } };
    await $writer;
    ok all(@ok), 'readers see only values stored at that index';
    is @shared[15], 1_999 + 0 * 1, 'the last store to an index is visible after await';
    is @shared[0], 2_000, 'and so is the last store overall';
}

# Growth: a push reallocates the node while readers index into it.
{
    my @data = ^8;
    my @shared := @data;
    my $writer = start { push-onto(@shared, $_) for 8..1_000; True };
    my @ok = await (^3).map: { start {
        my $ok = True;
        for ^20_000 -> $i {
            my $v = @shared[$i +& 7];
            $ok = False unless $v == $i +& 7;
        }
        $ok
    } };
    await $writer;
    ok all(@ok), 'readers of the stable prefix are unaffected by growth';
    is @shared.elems, 1_001, 'every push landed';
    is @shared[1_000], 1_000, 'and the last pushed element is visible after await';
}
