use v6;
use Test;

# Track C: scalar hash element assignment (`%h{$k} = $v`) inside a `start` block
# writes through the shared cell, so concurrent threads all land instead of each
# mutating a private snapshot (last-writer-wins). See memory track-c-shared-thread-cells.

plan 7;

# --- 50 threads each writing one distinct key -> 50 elements ---
for ^3 {
    my %h;
    await (^50).map: -> $i { start { %h{$i} = $i * $i } };
    is %h.elems, 50, 'concurrent distinct-key hash writes all land';
}

# --- values are correct, not just the count ---
{
    my %h;
    await (^50).map: -> $i { start { %h{$i} = $i * $i } };
    is %h{7}, 49, 'concurrent hash write stores the correct value';
    is ([+] %h.values), 40425, 'sum of all concurrently-written values is exact';
}

# --- pre-seeded hash: parent element survives alongside thread writes ---
{
    my %h;
    %h<seed> = -1;
    await (^50).map: -> $i { start { %h{$i} = $i } };
    is %h.elems, 51, 'pre-seeded element survives concurrent writes';
}

# --- heavy contention: 20 threads x 50 keys each -> 1000 elements ---
{
    my %h;
    await (^20).map: -> $t { start { for ^50 -> $k { %h{"$t-$k"} = $t * $k } } };
    is %h.elems, 1000, 'heavy multi-key contention loses no updates';
}
