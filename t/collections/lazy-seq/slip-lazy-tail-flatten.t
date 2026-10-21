use Test;

# A lazy-tail Slip (the Slip of a `.lazy` list, which carries the list whole)
# flattens into the eager consumers Rakudo flattens it into: a `.map`
# callback's result, `take` and a `flat` argument (#13117). mutsu kept the
# lazy list as one element there, rendered `(...)`. Expected values are the
# `raku` (v2026.09) output.

plan 5;

sub lz(*@v) { (gather { take $_ for @v }).lazy }

is (1, 2).map({ slip(lz($_, $_)) }).raku, '(1, 1, 2, 2).Seq',
    'a map callback result flattens a lazy-tail Slip';
is (1, 2).map({ |lz($_, $_) }).raku, '(1, 1, 2, 2).Seq',
    '... also one written as |EXPR';
is (1, 2).map(-> $a, $b { slip(lz($a, $b)) }).raku, '(1, 2).Seq',
    'a multi-parameter map callback result flattens it';
is (gather { take slip(lz(5, 6)) }).List.raku, '(5, 6)', 'take flattens it';
is flat(1, slip(lz(5, 6))).raku, '(1, 5, 6).Seq', 'flat flattens it';
