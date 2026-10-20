use Test;

# ADR-14032: `.^can`, `.^method_table` and the `augment` conflict answer from
# Rakudo's method-table snapshot, gated by the method rows.

plan 7;

# Str, Cool and Mu each declare an `Int`.
is Str.^can("Int").elems, 3, 'Str.^can("Int") reaches Str, Cool and Mu';
ok Cool.^method_table<Int>:exists, 'Cool declares Int';

# Cool's own Int is reached by its owner only: a subset of Int still
# answers the type object.
is UInt.Int.^name, 'UInt', 'UInt.Int is the type object';
is "12".Int, 12, 'a Str coerces through its own Int';

is FatRat.^can("numerator").elems, 1, 'FatRat declares numerator';

# Rakudo declares the method, so an augment may not redeclare it, whether
# or not mutsu implements it.
throws-like q[use MONKEY; augment class Any { method iterator-and-first { } }],
    Exception, message => /'already has a method'/,
    'augment conflict reads the snapshot';
lives-ok { EVAL q[use MONKEY; augment class Any { method no-such-method-here { 42 } }] },
    'a name Rakudo does not declare can be added';
