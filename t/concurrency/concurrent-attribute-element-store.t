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

plan 7;

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

# A mutating METHOD on the same attribute container. This arrives as an
# ordinary value dispatch (`exec_call_method_op` -> `call_method_with_values`,
# verified by breakpoint) and reaches none of the store funnels, so it needs its
# own exclusion.
{
    class Pushy { has @.log is rw; }
    my $p = Pushy.new(log => []);
    sub add($v) { $p.log.push($v) }
    await (^20).map: -> $t { start { for ^50 -> $k { add($t * 50 + $k) } } };
    is $p.log.elems, 1000, 'every push onto an attribute container lands';
}

# ADR-0068 §4 step 3, the last route the ticket left open: the container is
# handed back by a USER-WRITTEN accessor (`method bag() { @!items }`) rather
# than a generated one. It was measured landing 304-543 of 1000 writes.
#
# Two things were wrong, and only the first is a locking question. The accessor
# assignment rebound the attribute to a fresh container instead of storing into
# the one it already held (pinned deterministically in
# `t/attribute-accessor-container-identity.t`), so the container's address moved
# on every write and the store guard, keyed on it, locked a different stripe
# each time. And the guard was taken only *after* the accessor ran, leaving the
# read of the live container outside the region. The guard is now keyed on the
# invocant's attribute cell -- the one address every thread agrees on -- and
# acquired before the accessor dispatch.
{
    class Bagged { has @.items; method bag() { @!items } }
    my $b = Bagged.new;
    sub put-it($i) { $b.bag[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-it($t * 50 + $k) } } };
    is $b.items.grep(*.defined).elems, 1000,
        'every element store through a user-written array accessor lands';
}

{
    class HBagged { has %.items; method bag() { %!items } }
    my $b = HBagged.new;
    sub put-key($i) { $b.bag{"k$i"} = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-key($t * 50 + $k) } } };
    is $b.items.elems, 1000,
        'every key store through a user-written hash accessor lands';
}

# The same shape written straight into the thread body, with no named sub in
# between. This is the variant that stayed exposed after the first attribute
# slice: with no routine to close over the invocant it never became celled, so
# the celled read/write guard never applied to it either.
{
    class Inline { has @.rows is rw; }
    my $n = Inline.new(rows => []);
    await (^12).map: -> $t { start { for ^40 -> $k { $n.rows[$t * 40 + $k] = 1 } } };
    is $n.rows.grep(*.defined).elems, 480,
        'an inline attribute element store lands with no routine in between';
}
