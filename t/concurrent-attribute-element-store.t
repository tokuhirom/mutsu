use Test;

# ADR-0068 §3 route 3 -- an object attribute written from several threads --
# was recorded as "Unresolved: the probe reached none of the aliased-store
# sites this session probed". Measured 2026-09-06 with the §1.1 harness it is
# not merely exposed, it is the worst route yet found: 96 of 96 runs corrupted
# the heap (`corrupted size vs. prev_size`, `double free or corruption (top)`,
# a NaN-box tag panic), with the name-keyed lane never consulted -- which is
# expected, since an attribute-rooted store is not name-keyed at all.
#
# The route reaches its container through `builtin_index_assign_method_lvalue`
# (`$obj.attr[$i] = v` lowers to `__mutsu_index_assign_method_lvalue`), and
# every write in that function goes through the container the accessor hands
# back -- so one exclusion on that container covers them all.
#
# Like the other cross-thread pins, these assert mutsu's stronger guarantee:
# rakudo does not promise that concurrent unsynchronised writes to one
# container all land, but it does not corrupt its own heap either, and every
# row below is measured green under raku.

plan 3;

# The array attribute: 1000 distinct indices from 20 threads.
{
    class Holder { has @.seen is rw; }
    my $h = Holder.new(seen => []);
    sub note-array($v) { $h.seen[$v] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { note-array($t * 50 + $k) } } };
    is $h.seen.grep(*.defined).elems, 1000,
        'every element store through an array attribute lands';
}

# The hash attribute takes the same path.
{
    class HHolder { has %.seen is rw; }
    my $h = HHolder.new(seen => {});
    sub note-hash($v) { $h.seen{"k$v"} = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { note-hash($t * 50 + $k) } } };
    is $h.seen.elems, 1000, 'every key store through a hash attribute lands';
}

# Reached through a named sub the thread body merely calls, which is the shape
# the thread-escape analysis cannot see (ADR-0039 §8.6) -- the same shape that
# made the celled-container route reproduce on the first run.
{
    class Nested { has @.rows is rw; }
    my $n = Nested.new(rows => []);
    sub deep($i) { $n.rows[$i] = $i }
    await (^12).map: -> $t { start { for ^40 -> $k { deep($t * 40 + $k) } } };
    is $n.rows.grep(*.defined).elems, 480,
        'the same holds when the thread body only calls a routine that writes';
}
