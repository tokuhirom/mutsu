use Test;

# ADR-0068 §4 step 3: a CHAINED subscript store (`%h<k>[$i] = v`) mutates the
# inner container through `gc_contents_mut` exactly as a single-subscript store
# does, but it was on none of the three guarded funnels. Measured with the §1.2
# breakpoint oracle, this shape hit zero of them, and 20 threads writing 1000
# elements SIGSEGV'd on 24 of 24 runs where rakudo answers 1000.
#
# The container is reached through a named sub the thread body merely calls --
# a route the closure-capture analysis cannot see (ADR-0039 §8.6), which is what
# keeps it off the celled lane and on the raw store.

plan 5;

{
    my %h = k => [];
    sub put-hash-array($i) { %h<k>[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-hash-array($t * 50 + $k) } } };
    is %h<k>.grep(*.defined).elems, 1000, 'a `%h<k>[$i]` store from 20 threads keeps every write';
}

{
    my @a = [[], []];
    sub put-array-array($i) { @a[0][$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-array-array($t * 50 + $k) } } };
    is @a[0].grep(*.defined).elems, 1000, '... and so does `@a[0][$i]`';
}

# The single-subscript route this one was modelled on must stay fixed.
{
    my @b;
    sub put-plain($i) { @b[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-plain($t * 50 + $k) } } };
    is @b.grep(*.defined).elems, 1000, 'the single-subscript named store is still exclusive';
}

# The store's read-only probes (the Proxy probe, the Pair/`is default` peeks,
# the typed-autoviv check) walked the root and its inner level with nothing
# held, while another thread resized that inner level in place under the
# root's stripe. Under load that was a `double free or corruption` or a
# `Gc::make_mut strong-count underflow` about once in 35 runs of this block
# (#11538). The probes now hold the root's stripe too.
{
    my %h = k => [];
    sub put-hash-array-hot($i) { %h<k>[$i] = 1 }
    await (^16).map: -> $t { start { for ^500 -> $k { put-hash-array-hot($t * 500 + $k) } } };
    is %h<k>.grep(*.defined).elems, 8000, '16 threads storing `%h<k>[$i]` keep every write';

    my @a = [[], []];
    sub put-array-array-hot($i) { @a[0][$i] = 1 }
    await (^16).map: -> $t { start { for ^500 -> $k { put-array-array-hot($t * 500 + $k) } } };
    is @a[0].grep(*.defined).elems, 8000, '... and so do 16 threads storing `@a[0][$i]`';
}
