use v6;
use Test;

# Issue #8497: `multi_dim_index_read` (src/vm/vm_var_multidim_ops.rs) already
# handled a bare `Seq` element under a further dimension (`@a.map({...})[*;*]`),
# but not one that reached the array element store `$`-itemized (a `Seq` is
# wrapped in a genuine `Scalar` container by `Value::item()`, unlike
# Array/Hash/Slip, which carry their itemization as a same-repr flag) or
# boxed in a cross-frame `ContainerRef` cell (Track B) -- both fell into the
# "wrap the whole thing as a single-element list" fallback meant for a true
# leaf, so `outer[dim; i]` returned the WHOLE itemized Seq unchanged for
# `i == 0` and Nil for every other `i`.
#
# Found via Game::Entities 0.1.6's `Game::Entities.sort($c, $comparator)`,
# which stores a `.sort`-produced Seq into an array element with `.=` and
# later reads it back with `$pool[DENSE; $i]`.

plan 8;

# Sanity control: reading through the same cross-frame cell without any
# itemized Seq involved is unaffected.
{
    my @plain;
    sub setup0() { @plain = "sparse", [3, 5, 1], "components"; }
    setup0();
    is @plain[1; 0], 3, 'plain nested-array multidim read (control, unaffected)';
}

# The itemized-Seq-as-plain-element shape (`$(Seq)` stored directly, no cell).
{
    my @outer = "sparse", $((3, 5, 1).Seq), "components";
    is @outer[1; 0], 3, 'multidim index into a directly-itemized Seq (i=0)';
    is @outer[1; 1], 5, '...i=1';
    is @outer[1; 2], 1, '...i=2';
}

# The shape that actually reaches this in Game::Entities: a `.=`-compound
# assignment stores the Seq into a real array element, and the outer array
# has already been promoted to a cross-frame `ContainerRef` cell (Track B).
{
    my @outer;
    sub setup() { @outer = "sparse", (3, 5, 1), "components"; }
    setup();
    @outer[1] .= sort: { $^a <=> $^b };
    is @outer[1].raku, '$((1, 3, 5).Seq)', 'sanity: .= sort stores an itemized Seq';
    is @outer[1; 0], 1, 'multidim index through a cross-frame cell + itemized Seq (i=0)';
    is @outer[1; 1], 3, '...i=1';
    is @outer[1; 2], 5, '...i=2';
}
