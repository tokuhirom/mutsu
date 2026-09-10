use Test;

# `Z`/`X` metaops can use any infix as their base, including the range
# operators (`..`, `..^`, `^..`, `^..^`). raku baseline:
#   (1, 2) Z.. (5, 6)      -> (1..5 2..6)
#   (1, 2) X.. (5, 6)      -> (1..5 1..6 2..5 2..6)
#   (1, 2) Z^..^ (5, 6)    -> (1^..^5 2^..^6)

plan 7;

is-deeply ((1, 2) Z.. (5, 6)).list, (1..5, 2..6), 'Z.. zips into inclusive ranges';
is-deeply ((1, 2) Z..^ (5, 6)).list, (1..^5, 2..^6), 'Z..^ zips into end-exclusive ranges';
is-deeply ((1, 2) Z^.. (5, 6)).list, (1^..5, 2^..6), 'Z^.. zips into start-exclusive ranges';
is-deeply ((1, 2) Z^..^ (5, 6)).list, (1^..^5, 2^..^6), 'Z^..^ zips into both-exclusive ranges';

is-deeply ((1, 2) X.. (5, 6)).list, (1..5, 1..6, 2..5, 2..6), 'X.. crosses into inclusive ranges';

# the built ranges are real, iterable Ranges
is-deeply ((1, 2) Z.. (3, 4)).map(*.list).list, ((1, 2, 3), (2, 3, 4)), 'Z..-built ranges iterate';

# `R..` (a distinct, non-Z/X reverse metaop) keeps working alongside this fix
is-deeply (4 R.. 5), (5 .. 4), 'R.. still swaps endpoints';
