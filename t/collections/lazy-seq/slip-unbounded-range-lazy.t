use Test;

plan 12;

# Slipping an unbounded Range into a list literal used to flatten it eagerly
# up to a 1M-element prefix, so `my @a = 0, |(1..*)` was a finite, non-lazy
# Array (#10862). The Range now rides the Slip as its `.succ`-stepping lazy
# list, and the literal becomes a lazy concatenation.

{
    my @a = 0, |(1..*);
    ok @a.is-lazy, 'an Array with a slipped 1..* is lazy';
    is-deeply @a[^3], (0, 1, 2), 'its elements follow the prefix';
    throws-like { @a.elems }, X::Cannot::Lazy, '.elems on it throws';
}

ok (0, |(1..*)).is-lazy, 'a List with a slipped 1..* is lazy';
is-deeply (0, |(1..*))[^4], (0, 1, 2, 3), 'and reads lazily';
ok [0, |(1..*)].is-lazy, 'so is a bracketed Array literal';

{
    my @a = 0, |("a" .. *);
    is-deeply @a[^3], (0, "a", "b"), 'a Str-start unbounded range slips lazily';
}

ok (0, |(1..Inf)).is-lazy, 'an Inf endpoint is unbounded too';
is-deeply (0, |(^Inf))[^3], (0, 0, 1), '^Inf slips lazily';

{
    my @a = |(1..*), 5;
    is-deeply @a[^2], (1, 2), 'a slipped unbounded range at the front';
}

{
    my @seen;
    for 0, |(1..*) { last if $_ > 2; @seen.push($_) }
    is-deeply @seen, [0, 1, 2], 'a for loop over the lazy literal stops on last';
}

is-deeply (0, |(1..5)), (0, 1, 2, 3, 4, 5), 'a bounded range still slips eagerly';
