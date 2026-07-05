use Test;

# Slipping a `gather` with `|` must run the gather's body (side effects and all)
# and yield the taken values — reading its still-empty cache made
# `|(gather { … })` slip nothing and skip the side effects. An explicitly-`lazy`
# gather stays lazy: its body runs only on later reification (`.eager`).

plan 8;

# --- a plain gather is forced when slipped: values AND side effects ---
{
    my $ran = 0;
    my @a = |(gather { $ran++; take 5; take 6 });
    is @a.elems, 2, 'slip of a plain gather yields its taken values';
    is @a[1], 6, 'slipped gather values are correct';
    is $ran, 1, 'the plain gather body ran (side effect fired)';
}

# --- a slipped gather with no take still runs its side effect ---
{
    my $ran = 0;
    my @a = |(gather { $ran++ }), 1;
    is-deeply @a, [1], 'empty gather slips nothing';
    is $ran, 1, 'the empty gather body still ran';
}

# --- an explicitly-lazy gather is NOT forced by the slip ---
{
    my $marker = 0;
    my @a = |(gather { $marker++ }), 1, |(lazy gather { $marker++ });
    is $marker, 1, 'only the non-lazy gather reified on assignment';
    @a.eager;
    is $marker, 2, 'eagerizing reifies the preserved lazy gather tail';
}

# --- an infinite scan reduction slipped with | still reifies on demand ---
{
    my \d = 0, |[\+] 1..*;
    is d[5], 15, 'prefix | on an infinite scan reduction reifies on index';
}
