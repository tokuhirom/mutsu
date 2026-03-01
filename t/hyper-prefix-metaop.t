use Test;

plan 4;

my @hyp = -« ([1, 2], 3);
is @hyp.item.raku, '[[-1, -2], -3]', 'unicode hyper prefix -« preserves nested list shape';

my @hyp_ascii = -<< ([1, 2], 3);
is @hyp_ascii.item.raku, '[[-1, -2], -3]', 'ascii hyper prefix -<< matches unicode form';

my @bool = ?« (0, 5, "");
is @bool.join(" "), "False True False", '?« applies boolify element-wise';

my @stringified = ~« (1, 2);
is @stringified.join(" "), "1 2", '~« applies stringify element-wise';
