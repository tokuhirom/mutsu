use Test;

# A one-element Slip renders like a one-element list -- with the trailing
# comma that keeps it round-trippable: `slip(3,)`, not `slip(3)` (which would
# EVAL back to the same thing here, but is not what Rakudo emits and reads as
# a call with no comma). An empty Slip is `Empty`.

plan 8;

is slip(3).raku, 'slip(3,)', 'a one-element slip keeps the trailing comma';
is slip("a").raku, 'slip("a",)', 'a one-element Str slip';
is Slip.new(3).raku, 'slip(3,)', 'Slip.new with one element';
is (3,).Slip.raku, 'slip(3,)', '.Slip on a one-element list';
is slip(1, 2).raku, 'slip(1, 2)', 'a multi-element slip has no trailing comma';
is slip().raku, 'Empty', 'an empty slip is Empty';

# A slipped element still flattens into its container, so the container's own
# rendering is unaffected.
is (slip(3),).raku, '(3,)', 'a slip flattens into a list';
is [slip(3)].raku, '[3]', 'a slip flattens into an array';

done-testing;
