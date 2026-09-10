use v6;
use Test;

# `.pairup` walks its source list: a Pair element passes through unchanged
# (consuming one element), any other value takes the following element as its
# value (`key => value`, consuming two). A bare type object listifies to
# nothing, and a trailing lone non-Pair element is an error.

plan 12;

# A Pair in the list is emitted as-is; the rest pairs up two at a time.
is (a => 1, 'b', 'c').pairup.raku, '(:a(1), :b("c")).Seq', 'leading Pair then key/value';
is (a => 1, b => 2).pairup.raku,   '(:a(1), :b(2)).Seq',   'all Pairs pass through';
is ('k', 'v', c => 3).pairup.raku, '(:k("v"), :c(3)).Seq', 'key/value then trailing Pair';

# Plain values pair up two at a time.
is (1, 2, 3, 4).pairup.raku, '(1 => 2, 3 => 4).Seq', 'plain values chunk into pairs';
is ('x', 'y').pairup.raku,   '(:x("y"),).Seq',       'single key/value pair';
is ().pairup.raku,           '().Seq',               'empty list is empty';

# A bare type object listifies to nothing.
is Range.pairup.raku, '().Seq', 'Range type object is empty';
is Int.pairup.raku,   '().Seq', 'Int type object is empty';
is Any.pairup.raku,   '().Seq', 'Any type object is empty';

# A trailing lone non-Pair element is an odd-count error.
throws-like { (1, 2, 3).pairup.eager }, X::Pairup::OddNumber,
    'odd plain count throws', message => /'Odd number of elements found for .pairup()'/;
throws-like { (a => 1, 'b').pairup.eager }, X::Pairup::OddNumber,
    'Pair then lone value throws';

# A Pair does not consume a following value, so this stays even.
is (a => 1, 'b', 'c', d => 4).pairup.raku, '(:a(1), :b("c"), :d(4)).Seq',
    'Pairs interspersed with key/value pairs';
