use v6;
use Test;

# Track C: scalar array element index-assignment (`@a[$i] = $v`) inside a `start`
# block writes through the shared cell, so concurrent threads all land instead of
# each mutating a private snapshot (last-writer-wins). The `@a.push` counterpart
# already worked; this is the index-assign sibling of the shared hash element
# write. See memory track-c-shared-thread-cells.

plan 7;

# --- 50 threads each writing one distinct index -> 50 defined elements ---
for ^3 {
    my @a;
    await (^50).map: -> $i { start { @a[$i] = $i * $i } };
    is @a.grep(*.defined).elems, 50, 'concurrent distinct-index array writes all land';
}

# --- values are correct, not just the count ---
{
    my @a;
    await (^50).map: -> $i { start { @a[$i] = $i * $i } };
    is @a[7], 49, 'concurrent array write stores the correct value';
    is ([+] @a), 40425, 'sum of all concurrently-written elements is exact';
}

# --- pre-populated array: parent elements survive alongside thread writes ---
{
    my @a = 0 xx 5;
    await (^5).map: -> $i { start { @a[$i] = $i + 100 } };
    is @a.sum, 510, 'pre-populated slots overwritten by concurrent writes';
}

# --- heavy contention: 20 threads x 50 contiguous indices each ---
{
    my @a;
    await (^20).map: -> $t { start { for ^50 -> $k { @a[$t * 50 + $k] = 1 } } };
    is @a.grep(*.defined).elems, 1000, 'heavy contiguous-index contention loses no updates';
}
