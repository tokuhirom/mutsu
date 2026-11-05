use Test;

# `.chrs` takes the integers of its elements; the numeric words of a `< >`
# list are `IntStr` allomorphs, which are Ints.

plan 6;

is <67 97 109 101 108 105 97>.chrs, 'Camelia', 'IntStr elements from a word list';
is <65 66>.chrs, 'AB', 'a short one';
is (<72> , <105>).chrs, 'Hi', 'IntStr elements from a list of lists';
is <67 97>.map(*.Int).chrs, 'Ca', 'plain Ints still work';
is (^3).map({ <7 8 9>[$_] }).chrs.raku, "\"\\x[7]\\b\\t\"", 'IntStr elements picked out of a word list';
is <72 105>.chrs.WHAT.raku, 'Str', 'the result is a Str';
