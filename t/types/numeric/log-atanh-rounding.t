use Test;

plan 9;

is log10(1001).raku, '3.0004340774793183e0', 'log10 routine uses the natural-log ratio';
is 1001.log10.raku, '3.0004340774793183e0', 'log10 method shares the routine result';
is log10(1001e0).raku, '3.0004340774793183e0', 'Num log10 has the same rounding';
is atanh(0.5).raku, '0.5493061443340549e0', 'atanh routine uses the setting formula';
is 0.5e0.atanh.raku, '0.5493061443340549e0', 'atanh method shares the routine result';
is tanh(atanh(0.5)).raku, '0.5000000000000001e0', 'tanh sees the correctly rounded atanh';
is atanh(-0.5).raku, '-0.5493061443340549e0', 'negative atanh preserves the sign';
is atanh(1e0).raku, 'Inf', 'atanh of one is positive infinity';
is atanh(2e0).raku, 'NaN', 'atanh outside the real domain is NaN';
