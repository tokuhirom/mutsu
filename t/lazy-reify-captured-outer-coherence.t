use Test;

# Slice F coherence pin: a list method run with a block argument
# (`.map`/`.grep`/`.sort`/`.first`) — and a `gather` body — can mutate a
# *captured-outer* caller lexical (`$c` in `@a.map({ $c++ })`). The native block
# loop / gather reify writes the new value into env, but the caller's local slot
# was previously only reconciled by the blanket reverse `sync_locals_from_env`
# pull. These cases must stay coherent WITHOUT that pull (run the file with
# MUTSU_NO_REVERSE_SYNC=1 to verify the write-through reconcile at the call site).

plan 12;

# 1. .map over a List literal, captured-outer counter.
{
    my $c = 0;
    my @r = (1, 2, 3).map({ $c++; $_ * 2 });
    @r.elems;
    is $c, 3, '.map over a List mutates a captured-outer counter';
}

# 2. .map over a real @-array variable.
{
    my @a = 1, 2, 3;
    my $c = 0;
    @a.map({ $c++; $_ }).eager;
    is $c, 3, '.map over an @-array mutates a captured-outer counter';
}

# 3. .grep captured-outer counter.
{
    my $c = 0;
    my @r = (1, 2, 3).grep({ $c++; $_ > 0 });
    @r.elems;
    is $c, 3, '.grep mutates a captured-outer counter';
}

# 4. .sort with a comparator block.
{
    my $c = 0;
    (3, 1, 2).sort({ $c++; $^a <=> $^b });
    ok $c >= 2, '.sort comparator mutates a captured-outer counter';
}

# 5. .first runs the block until a match.
{
    my $c = 0;
    (1, 2, 3).first({ $c++; $_ > 5 });
    is $c, 3, '.first mutates a captured-outer counter (no match: all visited)';
}

# 6. gather/take with a captured-outer counter, forced via .elems.
{
    my $c = 0;
    my @g = gather { for 1..3 { $c++; take $_ } };
    @g.elems;
    is $c, 3, 'gather body mutates a captured-outer counter';
}

# 7. .map accumulating into a captured-outer array.
{
    my @seen;
    (1, 2, 3).map({ @seen.push($_); $_ }).eager;
    is @seen.elems, 3, '.map captured-outer array push is visible to caller';
}

# 8. .map accumulating into a captured-outer sum.
{
    my $sum = 0;
    (1, 2, 3, 4).map({ $sum += $_; $_ }).eager;
    is $sum, 10, '.map captured-outer += is visible to caller';
}

# 9. nested: outer .map whose block calls an inner .map mutating the same var.
{
    my $c = 0;
    (1, 2).map({ (10, 20).map({ $c++; $_ }).eager; $_ }).eager;
    is $c, 4, 'nested .map mutates the shared captured-outer counter';
}

# 10. .grep result count is still correct (functional behavior intact).
{
    my $c = 0;
    my @r = (1, 2, 3, 4, 5).grep({ $c++; $_ %% 2 });
    is @r.elems, 2, '.grep still returns the correct filtered list';
}

# 11. .map result values are still correct.
{
    my $c = 0;
    my @r = (1, 2, 3).map({ $c++; $_ * 10 });
    is @r.join(','), '10,20,30', '.map still returns the correct mapped values';
}

# 12. counter read mid-pipeline after a forced force.
{
    my $c = 0;
    my @r = (1, 2, 3).map({ $c++; $_ });
    @r.elems;
    my $first = $c;
    @r.elems;
    is $first, 3, 'captured-outer counter is final after the first force';
}
