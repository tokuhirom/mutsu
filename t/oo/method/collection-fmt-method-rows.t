use Test;

# fmt on the collections is one implementation behind rows of the method table
# (ADR-11276 §9.38): the zero-, one- and two-argument forms agree with Rakudo.

plan 26;

# --- no argument
is [1, 2, 3].fmt, '1 2 3', 'Array.fmt';
is (1, 2, 3).fmt, '1 2 3', 'List.fmt';
is (1..3).fmt, '1 2 3', 'Range.fmt';
is (1, 2).Seq.fmt, '1 2', 'Seq.fmt';
is (a => 1).fmt, "a\t1", 'Pair.fmt';
is { a => 1 }.fmt, "a\t1", 'Hash.fmt';
is Map.new((a => 1)).fmt, "a\t1", 'Map.fmt';
is ((a => 1), (b => 2)).fmt, "a\t1 b\t2", 'a list of Pairs';

# --- format
is [1, 2, 3].fmt('%03d'), '001 002 003', 'Array.fmt($format)';
is (1..3).fmt('%d!'), '1! 2! 3!', 'Range.fmt($format)';
is (a => 1).fmt('%s=%s'), 'a=1', 'Pair.fmt($format)';
is { a => 1 }.fmt('%s:%d'), 'a:1', 'Hash.fmt($format)';
is ((a => 1), (b => 2)).fmt('%s=%s'), 'a=1 b=2', 'a list of Pairs with a format';
is (1, 2).Seq.fmt('<%s>'), '<1> <2>', 'Seq.fmt($format)';
is [].fmt('%d'), '', 'an empty Array';
is SetHash.new(<a>).fmt('%s-%s'), 'a-True', 'SetHash.fmt($format)';
is Bag.new(<a a>).fmt('%s-%s'), 'a-2', 'Bag.fmt($format)';

# --- format and separator
is [1, 2, 3].fmt('%d', ', '), '1, 2, 3', 'Array.fmt($format, $separator)';
is (1..3).fmt('%d', '-'), '1-2-3', 'Range.fmt($format, $separator)';
is { a => 1, b => 2 }.fmt('%s=%s', ';').split(';').sort.join(','), 'a=1,b=2', 'Hash.fmt($format, $separator)';
is (1, 2).Seq.fmt('%d', '+'), '1+2', 'Seq.fmt($format, $separator)';
is Bag.new(<a>).fmt('%s-%s', '|'), 'a-1', 'a one-element Bag with a separator';

# --- the scalar forms keep their answers
is 5.fmt('%03d'), '005', 'Int.fmt($format)';
is 'a'.fmt('%s!'), 'a!', 'Str.fmt($format)';
is 5.fmt, '5', 'Int.fmt';
throws-like { 5.fmt('%d', ',') }, Exception, '.fmt($format, $separator) on a scalar';

# vim: expandtab shiftwidth=4
