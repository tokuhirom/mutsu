use Test;
plan 6;

# A `my @x = seed…, -> $p { … @x … } … *` sequence whose generator consults the
# sequence being built is *re-entrant*: eager prefix generation would read @x
# before the assignment binds it. mutsu defers such a sequence to lazy so the
# generator runs on demand (after @x is bound), matching Rakudo.
# (roast/integration/advent2012-day14.t)
{
    my @primes = 2, 3, 5, -> $p { ($p+2, $p+4 ... &is-prime)[*-1] } ... *;
    sub is-prime($n) { $n %% none @primes ...^ * > sqrt $n }
    is @primes[^8].join(','), '2,3,5,7,11,13,17,19', 'mutually-recursive lazy prime sequence';
    is @primes[10], 31, 'deeper index extends the re-entrant sequence';
}

# `@primes ...^ pred` where `@primes` is the lazy sequence walks its own
# elements against the predicate instead of deducing a new pattern from the
# realized prefix (which would throw X::Sequence::Deduction on 2,3,5).
{
    my @s = 2, 3, 5, 7, 11 ... *;   # arithmetic-ish, but lazy/infinite
    is (@s ...^ * > 6).join(','), '2,3,5', 'lazy-list LHS of ...^ walks its elements';
}

# A generator that `slip`s multiple values contributes each as its own element,
# including past the eager prefix (the on-demand extension flattens Slips too).
{
    is (1, 1, { slip $^a + 1, $^b * 2 } ... *)[^12].join(' '),
       '1 1 2 2 3 4 4 8 5 16 6 32', 'slip generator flattens on lazy extension';
    is (1, { |($^n + 1 xx $^n + 1) } ... *)[^10].join(' '),
       '1 2 2 3 3 3 4 4 4 4', 'list-returning generator flattens on extension';
}

# A non-re-entrant infinite closure sequence is unaffected (still reifies).
{
    is (0, 1, *+* ... *)[^8].join(','), '0,1,1,2,3,5,8,13', 'plain fibonacci still reifies';
}
