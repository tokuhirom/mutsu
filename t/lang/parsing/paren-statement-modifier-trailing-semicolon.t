use Test;

plan 6;

# Logic::Ternary (ecosystem): the content of `( ... )` is a semilist of
# statements, so a modified statement may end with its own `;` before the `)`.
my $on = True;
my $off = False;

is (1, 2 if $on;).join('|'), '1|2', 'comma list with a modifier and a trailing ;';
is (|(3, 4) if $on;).join('|'), '3|4', 'slip with a modifier and a trailing ;';
is (5 if $off;).elems, 0, 'false condition gives the empty list';

my @nested = |(
    |(
        a => 1,
        b => 2,
    ) if $on;
);
is @nested.elems, 2, 'multi-line nested form, enabled';

my @none = |(
    |( a => 1, ) if $off;
);
is @none.elems, 0, 'multi-line nested form, disabled';

is (7 if $on), 7, 'the form without ; still works';
