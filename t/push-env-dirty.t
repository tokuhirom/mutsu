use v6;
use Test;

# Regression pin for Slice 6.3 step 2: the native `@a.push` fast path
# (vm_data_ops) no longer sets a blanket env_dirty. It mutates only the target
# array in env and reverse-write-throughs the result into the target's local
# slot, so the caller's slot stays coherent without an O(caller-locals) pull.
# A caller local interleaved with pushes in a loop must stay correct, and the
# interpreter-fallback push branches (shared/shaped/non-simple targets) still
# mark conservatively. See docs/vm-dual-store.md (Slice 6.3).

plan 13;

# --- basic accumulation ---
{
    my @a;
    for ^5 { @a.push($_) }
    is @a.join(','), '0,1,2,3,4', 'push accumulates in a loop';
    is @a.elems, 5, 'push elems correct';
}

# --- caller local interleaved with pushes stays coherent ---
{
    my @d;
    my $sum = 0;
    my $count = 0;
    for ^4 { @d.push($_); $sum = $sum + @d.elems; $count = $count + 1 }
    is $sum, 10, 'interleaved caller local accumulates (1+2+3+4)';
    is $count, 4, 'caller loop counter coherent across pushes';
    is @d.elems, 4, 'array length correct after loop';
}

# --- push multiple args / slip ---
{
    my @b;
    @b.push(1, 2, 3);
    is @b.join(','), '1,2,3', 'push of multiple values';
    my @s = 7, 8;
    @b.push(|@s);
    is @b.join(','), '1,2,3,7,8', 'push of a slip';
}

# --- push to a captured-outer array from a sub, in a loop ---
{
    my @log;
    sub record($x) { @log.push($x) }
    record($_) for <a b c>;
    is @log.join(','), 'a,b,c', 'captured-outer array push from sub';
}

# --- push return value is the array ---
{
    my @c;
    my $r = @c.push(9);
    is $r.elems, 1, 'push returns the array';
}

# --- nested push (inner + outer arrays) ---
{
    my @outer;
    for ^2 -> $i {
        my @inner;
        for ^2 -> $j { @inner.push($i * 10 + $j) }
        @outer.push(@inner.join('-'));
    }
    is @outer.join(','), '0-1,10-11', 'nested push keeps both arrays coherent';
}

# --- typed array push ---
{
    my Int @e;
    @e.push(1); @e.push(2);
    is @e.join(','), '1,2', 'typed Int array push';
}

# --- autovivified array push ---
{
    my @z;
    @z.push($_) for 10, 20, 30;
    is @z.sum, 60, 'pushed values usable in reductions';
}

# --- push interleaved with reading another array's elems ---
{
    my @x;
    my @y;
    for ^3 {
        @x.push($_);
        @y.push(@x.elems);
    }
    is @y.join(','), '1,2,3', 'two arrays pushed in lockstep stay coherent';
}
