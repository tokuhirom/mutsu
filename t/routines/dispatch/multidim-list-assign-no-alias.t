use v6;
use Test;

# `my ($a, $b) = @arr[DIM; $i, $j];` is a plain VALUE read: the parser wraps
# a non-`:=` list-assignment RHS in the synthetic `__mutsu_list_assign_rhs`
# helper, and a `MultiDimIndex` argument to a call was unconditionally
# compiled as a raw `\target` / `is rw` bind-ref (promoting the subscripted
# leaves to shared `ContainerRef` cells) -- even though this synthetic
# helper is a native value-only deitemizer, not a routine with a raw/`is rw`
# parameter. That silently turned a plain read into an aliasing side effect
# on the source array.
#
# A later swap-via-slice-assignment on the SAME two leaves
# (`@arr[DIM; $i, $j] = @arr[DIM; $j, $i];`) then wrote each promoted cell's
# *reference* into the other (the plain read's terminal case returns the
# stored value as-is, and an existing cell is written *through*), producing
# a two-cell reference cycle instead of swapping the two values. Reading the
# array back afterwards (`.raku`, further indexing) recursed forever.
#
# See https://github.com/tokuhirom/mutsu/issues/8552 (the Game::Entities
# `Game::Entities.sort($c, $comparator)` cycle-sort helper hit exactly this
# shape: `my ($ld, $rd) = $set[SPARSE; $le, $re];` followed by a swap on
# `$set[DENSE; $ld, $rd]`).

plan 5;

{
    # A single-element outer array composer flattens its lone nested-array
    # argument (`[ [...] ]` is `[...]`, the raku "one-arg rule"), so a second
    # placeholder element keeps `$set[0]` itemized as the inner array.
    my $set = [ [10, 20, 30, 40], Any ];
    my ( $i, $j ) = ( 0, 2 );
    # A plain list-assignment read of a multidim slice must not alias.
    is-deeply $set[0; $i, $j], (10, 30), 'list-assign read of a multidim slice yields plain values';
    $set[0; $i, $j] = $set[0; $j, $i];
    is-deeply $set[0].List, (30, 20, 10, 40), 'swap-via-slice-assignment after a list-assign read swaps correctly';
}

# The exact two-swap shape from the Game::Entities repro: a list-assign read
# of one dimension used to derive indices for a SECOND swap, mirroring
# `&swap`'s `$ld,$rd` -> `$ls,$rs` chain.
{
    my constant SPARSE = 0;
    my constant DENSE  = 1;

    my $set = [ [Any, 0, 1, 2, 3, 4], [1, 2, 3, 4, 5] ];
    my ( $le, $re ) = ( 1, 3 );

    my ( $ld, $rd ) = $set[ SPARSE; $le, $re ];
    my ( $ls, $rs ) = $set[ DENSE;  $ld, $rd ];

    $set[ DENSE; $ld, $rd ] = $set[ DENSE; $rd, $ld ];
    $set[ SPARSE; $ls, $rs ] = $set[ SPARSE; $rs, $ls ];

    is-deeply $set[DENSE].List, (3, 2, 1, 4, 5), 'DENSE swap after chained list-assign reads';
    # Index 0 (untouched, still the original `Any`) is compared via a direct
    # element read rather than folded into a `.List` `is-deeply`: converting
    # an Array that went through an element assignment to `.List` renders an
    # untouched `Any` slot as `Nil` in mutsu (a distinct, pre-existing,
    # general bug -- reproduces even for a single plain `@a[1] = 99` with no
    # multidim indexing at all -- unrelated to the aliasing bug this file
    # tests; see https://github.com/tokuhirom/mutsu/issues/8571).
    is $set[SPARSE][0], Any, 'SPARSE index 0 (untouched) stays Any';
    is-deeply $set[SPARSE][1..*].List, (2, 1, 0, 3, 4), 'SPARSE swap after chained list-assign reads';
}
