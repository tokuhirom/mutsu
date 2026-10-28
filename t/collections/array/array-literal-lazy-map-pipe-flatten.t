use Test;

# From Path::Finder: `[ find(...).map: { ... } ]`. A `.map` over a gather
# is a lazy pipe; the array literal flattened its (still empty) cache and
# produced `[]`.
plan 5;

sub g(*@d) {
    my @queue = @d;
    my $seq := gather while @queue { take @queue.shift }
    return $seq;
}

is-deeply [ g(1, 2, 3).map: { $_ * 2 } ], [2, 4, 6], 'map over a gather in [ ]';
is-deeply [ g(1, 2, 3).grep({ $_ != 2 }) ], [1, 3], 'grep over a gather in [ ]';
is-deeply [ (gather for 1..3 { take $_ }).map({ $_ + 1 }) ], [2, 3, 4], 'map over a gather for in [ ]';
is-deeply [ g(1, 2).map({ $_ * 3 }).map({ $_ + 1 }) ], [4, 7], 'chained pipes in [ ]';
is-deeply (my @a = g(1, 2, 3).map: { $_ * 2 }), [2, 4, 6], 'the assignment form is unchanged';

# vim: expandtab shiftwidth=4
