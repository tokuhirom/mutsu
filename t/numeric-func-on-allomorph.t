use Test;

# Numeric builtin *functions* applied to a numeric allomorph (IntStr / NumStr /
# RatStr / ComplexStr) must operate on the inner numeric value, exactly as the
# equivalent method does: `abs($x)` == `$x.abs`. Regression: several arms
# (abs/sqrt/floor/ceiling/round/exp) matched only scalar-numeric variants and
# fell to a `0`/`NaN` default for the `Mixin` allomorph, so `abs(<-2>)` was 0.

plan 18;

# abs
is abs(<-2>), 2, 'abs(IntStr)';
is abs(<-2.5>), 2.5, 'abs(RatStr) inner value';
is &abs.(<-2>), 2, 'abs as &-callable on allomorph';
is-deeply <-2 3 -4>.map(&abs).List, (2, 3, 4), 'map(&abs) over allomorph words';

# floor / ceiling / round / truncate
is floor(<-2.5>), -3, 'floor(RatStr)';
is ceiling(<-2.5>), -2, 'ceiling(RatStr)';
is round(<-2.4>), -2, 'round(RatStr)';
is truncate(<-2.5>), -2, 'truncate(RatStr)';

# exp / sqrt / log family
is exp(<0>), 1, 'exp(IntStr)';
is sqrt(<4>), 2, 'sqrt(IntStr)';
is log2(<8>), 3, 'log2(IntStr)';
is log10(<100>), 2, 'log10(IntStr)';

# sign
is sign(<-2>), -1, 'sign(IntStr)';

# function == method
is abs(<-7>), <-7>.abs, 'abs sub matches method on allomorph';
is floor(<-7.3>), <-7.3>.floor, 'floor sub matches method on allomorph';

# list/range-semantic functions are unaffected (sub form != method form)
is elems(<-2>), 1, 'elems(allomorph) still 1';
is-deeply combinations(3).List, ((), (0,), (1,), (2,), (0, 1), (0, 2), (1, 2), (0, 1, 2)),
    'combinations($n) still the powerset of ^$n';

# repeated/unique with an &-callable :as adverb over allomorph words.
# `.repeated` returns the original (allomorph) elements, so compare by value.
is <-2 3 -2 3>.repeated(:as(&abs)).map(*.Int).join(","), "-2,3",
    'repeated(:as(&abs)) over words';
