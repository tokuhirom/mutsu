use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token that calls the core `<trait>` rule gets a RakuAST::Trait for the
# one trait it matched (ADR-15762 slice 2, #13370).

plan 4;

use TraitShimSlang;

my @noted;
multi sub trait_mod:<is>(Routine:D $r, :$noted!) { @noted.push($r.name) }

fun plain is pure { 7 }
fun marked is noted { 8 }

is plain(), 7, 'a built-in trait is read';
is marked(), 8, 'a routine with a custom trait runs';
fun other is noted { 9 }
is other(), 9, 'a second routine with the trait';
is @noted.sort.join(','), 'marked,other', 'trait_mod:<is> ran for each routine that has the trait';
