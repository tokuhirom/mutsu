use Test;

# An element PRODUCER (`.values`, `.kv`, `.pairs`) hands out the source
# element's own `Scalar` container (ADR-0036 slice 3), so subscripting its `Seq`
# and assigning writes THROUGH the cell to the source.
#
# The computed-target spelling (`(@a.values)[0] = "x"`) already did that. The
# same `Seq` reached through a VARIABLE silently dropped the write: nothing in
# the named element-assign path knew how to store into a `Seq`, and the cell
# write-through lived only in the computed-target op. Both now share one
# helper (`try_seq_element_cell_assign`).
#
# Cross-checked against `raku` line by line.

plan 13;

# --- the computed-target spelling, which already worked ---------------------

{
    my @a = <A B>;
    (@a.values)[0] = "x";
    is-deeply @a, ["x", "B"], 'a computed `.values` subscript writes through';
}

{
    my @a = <A B>;
    (@a.kv)[1] = "x";
    is-deeply @a, ["x", "B"], '... and so does a computed `.kv` subscript';
}

# --- the named receiver, which did not --------------------------------------

{
    my @a = <A B>;
    my \s = @a.values;
    s[0] = "x";
    is-deeply @a, ["x", "B"], 'a sigilless-bound `.values` Seq writes through';
}

{
    my @a = <A B>;
    my $s = @a.values;
    $s[0] = "x";
    is-deeply @a, ["x", "B"], '... and a `$`-assigned one does too';
}

{
    my @a = <A B>;
    my \s := @a.values;
    s[0] = "x";
    is-deeply @a, ["x", "B"], '... and a sigilless `:=` bind of one';
}

{
    my @a = <A B>;
    my \s = @a.kv;
    s[1] = "x";
    is-deeply @a, ["x", "B"], '... and a named `.kv` Seq';
}

{
    my @a = <A B C>;
    my \s = @a.values;
    s[2] = "z";
    is-deeply @a, ["A", "B", "z"], 'a later index writes the right element';
}

{
    my @a = <A B C>;
    my \s = @a.values;
    s[0] = "x";
    s[1] = "y";
    is-deeply @a, ["x", "y", "C"], 'two writes through the same Seq both land';
}

# --- the reads that must keep working ---------------------------------------

{
    my @a = <A B>;
    my \s = @a.values;
    is s[0], "A", 'reading the Seq element still reads the value';
    @a[0] = "changed";
    is s[0], "changed", '... and reads through the cell, so it sees a later write';
}

# --- what must NOT change ---------------------------------------------------

{
    # A plain List's elements are not containers, so its subscript assignment is
    # still refused rather than silently writing somewhere.
    my $l = (1, 2, 3);
    dies-ok { $l[0] = 9 }, 'a plain List subscript assignment still dies';
}

{
    # A Seq with no element containers (an ordinary lazy map) is refused, and
    # the refusal names the ELEMENT the store addressed -- rakudo's
    # "Cannot modify an immutable Int (2)". (This was a `todo` until the
    # element-keyed `Seq` store landed; see
    # news/2026-09/immutable-element-store-and-bind.md.)
    my @a = 1, 2, 3;
    my $s = @a.map(* + 1);
    throws-like { $s[0] = 99 }, X::Assignment::RO,
        message => /'Cannot modify an immutable Int (2)'/,
        'a non-producer Seq subscript assignment dies, naming the element';
    is-deeply @a, [1, 2, 3], '... and in any case never reaches the source array';
}
