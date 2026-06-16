use Test;

plan 13;

# Concurrent `%h{$k} = $v` from `start` blocks must not lose updates: each
# thread's element write has to land in the single shared hash, not clobber a
# stale snapshot via `set_shared_var` during env sync.
{
    my %h;
    my @p;
    for 1..20 -> $i {
        @p.push: start { %h{$i} = $i * 2 }
    }
    await @p;
    is %h.elems, 20, 'all concurrent hash element writes land';
    is %h.values.sum, (1..20).map(* * 2).sum, 'no hash values lost';
}

# Concurrent `@a[$i] = $v` from `start` blocks must not lose updates.
{
    my @a;
    @a[$_] = 0 for 0..19;
    my @p;
    for 0..19 -> $i {
        @p.push: start { @a[$i] = $i * 3 }
    }
    await @p;
    is @a.elems, 20, 'all array indices present';
    is @a.sum, (0..19).map(* * 3).sum, 'no array values lost';
}

# A redeclared `my %h` inside a loop body must not seed from a previous
# iteration's concurrent element writes (atomic-store leak).
{
    my @sums;
    for 1..3 {
        my %h;
        my @p;
        for 1..5 -> $i {
            @p.push: start { %h{"k$i"} = $i }
        }
        await @p;
        @sums.push: %h.elems;
    }
    is @sums.join(','), '5,5,5', 'redeclared shared hash does not leak across iterations';
}

# A redeclared `my @a` inside a loop body must not leak either.
{
    my @sums;
    for 1..3 {
        my @a;
        @a[$_] = 0 for 0..4;
        my @p;
        for 0..4 -> $i {
            @p.push: start { @a[$i] = $i + 1 }
        }
        await @p;
        @sums.push: @a.sum;
    }
    is @sums.join(','), '15,15,15', 'redeclared shared array does not leak across iterations';
}

# A parent-seeded hash keeps its initial keys alongside concurrent writes.
{
    my %h = a => 1, b => 2;
    my @p;
    for 1..5 -> $i {
        @p.push: start { %h{"k$i"} = $i }
    }
    await @p;
    is %h.elems, 7, 'seeded hash keeps initial keys plus concurrent writes';
    is %h<a>, 1, 'seeded hash initial value preserved';
}

# A parent-seeded array keeps its initial elements.
{
    my @a = 10, 20, 30;
    my @p;
    for 3..7 -> $i {
        @p.push: start { @a[$i] = $i * 100 }
    }
    await @p;
    is @a[0], 10, 'seeded array initial element preserved';
    is @a.grep(*.defined).elems, 8, 'seeded array gains concurrent writes';
}

# Sparse cross-thread writes (growing the array) all land.
{
    my @a;
    my @p;
    for 0..9 -> $i {
        @p.push: start { @a[$i * 2] = $i }
    }
    await @p;
    is @a.grep(*.defined).elems, 10, 'sparse concurrent array writes all land';
}

# An instance-attribute hash (`%!data`) written inside `start` blocks keeps its
# own per-instance state: sequential element writes within one instance must
# accumulate (they must not funnel into the name-keyed shared store, and the
# local copy must not be dropped between writes).
{
    my class Bag {
        has %.data;
        method set($k, $v) { %!data{$k} = $v }
    }
    my @p;
    for 1..3 -> $i {
        @p.push: start {
            my $b = Bag.new;
            $b.set("x", $i);
            $b.set("y", $i * 10);
            $b.data.elems;
        }
    }
    my @counts = await @p;
    is @counts.sort.join(','), '2,2,2', 'per-instance attribute hashes accumulate, do not cross-contaminate';
}

# A thread-local `my %h` writing several keys inside a `start` block accumulates
# them (no env-copy drop between sequential element writes).
{
    my @p;
    for 1..4 -> $i {
        @p.push: start {
            my %local;
            %local{"a"} = $i;
            %local{"b"} = $i;
            %local.elems;
        }
    }
    my @counts = await @p;
    is @counts.join(','), '2,2,2,2', 'thread-local hash accumulates sequential element writes';
}
