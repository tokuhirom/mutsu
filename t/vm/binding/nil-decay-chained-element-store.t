# ADR-0049 at a CHAINED subscript store: `Nil` is the one value a `=` store can
# never leave in an element slot. It decays to the default of the container it
# lands in -- and at a chain that is the ROW the earlier subscripts reach, not
# the root variable, so a row never inherits the root's `is default(...)`.
#
# The single-subscript twin has done this since ADR-0049 slice 4; the chained
# store did not, and wrote a raw `Nil` (#8966).
#
# Every expectation below was taken from real rakudo, not from mutsu's output.
use Test;
plan 13;

{
    # The ticket's repro. The row is a plain Array, so its own default is `Any`
    # -- reaching for the root's `is default(42)` would be just as wrong as
    # storing the `Nil`.
    my @d is default(42);
    @d[0] = [1, 2];
    @d[0][0] = Nil;
    is @d[0][0].raku, 'Any', 'a Nil store into a row of a defaulted array decays to the row default';
    is @d.raku, '[[Any, 2],]', 'and the row itself shows the decayed element';
}

{
    my %h is default(42);
    %h<x> = {};
    %h<x><y> = 1;
    %h<x><y> = Nil;
    is %h<x><y>.raku, 'Any', 'the hash-of-hash chain decays the same way';
}

{
    my %g;
    %g<a> = [1, 2];
    %g<a>[0] = Nil;
    is %g<a>.raku, '$[Any, 2]', 'hash-of-array: the positional leaf decays';
}

{
    my @ah;
    @ah[0] = {};
    @ah[0]<k> = 1;
    @ah[0]<k> = Nil;
    is @ah.raku, '[{:k(Any)},]', 'array-of-hash: the associative leaf decays';
}

{
    # A typed row decays to its own element type object, not to `Any`.
    my @t;
    @t[0] = Array[Int].new(1, 2);
    @t[0][0] = Nil;
    is @t[0][0].raku, 'Int', 'a typed row decays a Nil to its element type object';
}

{
    # A row with its OWN `is default(...)` wins -- both when the row is a
    # `:=`-bound shared cell and when it was assigned in by value.
    my @inner is default(7) = 1, 2;
    my @f;
    @f[0] := @inner;
    @f[0][0] = Nil;
    is @inner.raku, '[7, 2]', 'a := bound row decays to the bound array own default';

    my @rowdef;
    @rowdef[0] = my @r is default(9) = 1, 2;
    @rowdef[0][1] = Nil;
    is @rowdef[0][1].raku, '9', 'an assigned-in row keeps its own default too';
}

{
    # The 3+-level chain reaches the rule through its own op.
    my @z;
    @z[0][0][0] = 1;
    @z[0][0][0] = Nil;
    is @z[0][0][0].raku, 'Any', 'the deep chain decays a Nil at its leaf';
}

{
    # A row that does not exist yet is walk-created untyped, so its default is
    # the plain `Any` -- the decay must not be skipped just because the store
    # also autovivifies.
    my @u;
    @u[0][0] = Nil;
    is @u.raku, '[[Any],]', 'an autovivified row still decays the Nil it is created for';
}

{
    # The assignment EXPRESSION's value is the decayed one, not the Nil.
    my @e = [1, 2], [3, 4];
    my $res = (@e[0][0] = Nil);
    is $res.raku, 'Any', 'the chained store evaluates to the decayed value';
    is @e.raku, '[[Any, 2], [3, 4]]', 'and the store landed';
}

{
    # A `:=` bind REPLACES the element container instead of storing into it,
    # so there is no Scalar to decay against and `Nil` stays `Nil`.
    my @bind;
    @bind[0] = [1, 2];
    @bind[0][0] := Nil;
    is @bind[0][0].raku, 'Nil', 'a := bind of Nil at a chain is left alone';
}
