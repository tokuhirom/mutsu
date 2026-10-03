use Test;

plan 5;

# A gather whose body reads its own sequence through an iterator: the
# re-entrant pull answers from the elements already taken instead of
# restarting the body (which recursed until the stack overflowed).
{
    my \S := gather {
        my $it = S.iterator;
        take 1;
        take 2 * $it.pull-one;
        take 2 * $it.pull-one;
    };
    is S[^3].Str, '1 2 4', 'gather reads its own earlier elements via .iterator';
}

# Hamming-style smooth numbers (the Smooth::Numbers distribution's shape):
# several iterators over the sequence being produced, each lagging behind.
sub smooth(*@list) {
    my \Smooth := gather {
        my %i = (flat @list) Z=> (Smooth.iterator for ^@list);
        my %n = (flat @list) Z=> 1 xx *;
        loop {
            take my $n := %n{*}.min;
            -> \k { %n{k} = %i{k}.pull-one * k if %n{k} == $n } for @list;
        }
    }
}
is smooth(2, 3)[^15].Str, '1 2 3 4 6 8 9 12 16 18 24 27 32 36 48',
    '3-smooth numbers';
is smooth(2, 3, 5)[^15].Str, '1 2 3 4 5 6 8 9 10 12 15 16 18 20 24',
    'Hamming numbers';
is smooth(2)[^5].Str, '1 2 4 8 16', 'a single factor';

# Reading an element the body has not taken yet cannot be produced (Rakudo
# hangs here; mutsu reports the cycle).
{
    my \T := gather { my $it = T.iterator; my $x = $it.pull-one; take 1 };
    dies-ok { T[0] }, 'reading an element not yet taken dies instead of recursing';
}
