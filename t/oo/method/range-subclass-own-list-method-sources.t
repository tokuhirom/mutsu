use Test;

plan 12;

# A Range subclass that declares only `list` keeps the base type's own
# `iterator` (the carried range): the methods written on `self.iterator`
# read the range, those written on `self.list` read the subclass's `list`
# (measured against Rakudo 2026.09; #16022).
class S is Range { method list { (7, 8).list } }
my \s = S.new(0, 3);

is-deeply s.map(* + 1).List, (1, 2, 3, 4), 'map reads the range';
is-deeply s.grep(* > 1).List, (2, 3), 'grep reads the range';
is-deeply s.sort.List, (0, 1, 2, 3), 'sort reads the range';
is s.first(* > 1), 2, 'first reads the range';
is-deeply s.Seq.List, (0, 1, 2, 3), 'Seq reads the range';

is s.join, '78', 'join reads the subclass list';
is-deeply s.Array, [7, 8], 'Array reads it';
is-deeply s.List, (7, 8), 'List reads it';
is s.Str, '7 8', 'Str reads it';
is-deeply s.pick(*).sort.List, (7, 8), 'pick reads it';

is s.sum, 6, 'sum reads the range (and elems agrees with raku)';
is-deeply s.list, (7, 8), 'an explicit .list is the subclass\'s';
