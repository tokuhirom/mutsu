use v6.e.PREVIEW;
use Test;

# ADR-0067 slice 3a, native half. `.snitch` is declared
# `method snitch(\snitchee: &snitcher = &note)` — its invocant is RAW — and it
# hands that invocant straight back, so `$a.snitch = 5` notes the old value and
# then writes `5` through `$a`'s own container.
#
# This is the reason `.snitch` cannot take `.item`'s route: `.item` is erased at
# compile time to a plain store, which is sound only because `.item` is pure.
# `.snitch` has a side effect, so erasing the call would drop it.
#
# Verified byte-identical under `mutsu` and `raku`.

plan 12;

# --- the scalar invocant ----------------------------------------------------

{
    my $noted;
    my $a = 42;
    $a.snitch({ $noted = $_ }) = 5;
    is $noted, 42, '.snitch logged the invocant before the assignment';
    is $a, 5, 'and the assignment wrote through the raw invocant';
}

{
    my $a = 42;
    $a.snitch = 5;
    is $a, 5, 'the default (note) snitcher assigns through too';
}

# --- the element invocant spellings -----------------------------------------

{
    my @noted;
    my @a = 1, 2;
    @a[0].snitch({ @noted.push($_) }) = 9;
    is @noted[0], 1, 'an array element is snitched by value';
    is-deeply @a, [9, 2], 'and the write reaches the element';
}

{
    my @noted;
    my %h = a => 1, b => 2;
    %h<a>.snitch({ @noted.push($_) }) = 9;
    is @noted[0], 1, 'a hash entry is snitched by value';
    is %h<a>, 9, 'and the write reaches the entry';
    is %h<b>, 2, 'leaving its neighbour alone';
}

# --- the rvalue call is unchanged -------------------------------------------

{
    my $a = 42;
    my $seen;
    my $back = $a.snitch({ $seen = $_ });
    is $seen, 42, 'an rvalue .snitch still logs the invocant';
    is $back, 42, 'and still returns it';
    is $a, 42, 'without disturbing the variable';
    ok $a.snitch(-> $ { }) =:= $a, '.snitch hands back the very container';
}

done-testing;
