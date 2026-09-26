use Test;

plan 16;

# `[&name]` and its meta forms (`R`, `X`, `Z`, `»[&name]«`, `[[&name]]`) use
# the callable the `&name` TERM denotes -- a `my &op` lexical or a `&op`
# parameter -- not an `infix:<name>` operator of the same name. (#9464)

my &op = &infix:<+>;
is-deeply ((10,) Z[&op] (9,)), (19,), 'Z[&op] zips with a my &op';
is-deeply ((1, 2) X[&op] (10, 20)), (11, 21, 12, 22), 'X[&op] crosses with a my &op';
is-deeply ((1, 2) »[&op]« (10, 20)), (11, 22), '»[&op]« hypers with a my &op';
is ([[&op]] 1, 2, 3), 6, '[[&op]] reduces with a my &op';

my &cmp = &infix:<after>;
is-deeply ((10,) Z[&cmp] (9,)), (True,), 'a lexical &cmp is not the builtin cmp';
is (10 [&cmp] 9), True, '[&cmp] calls the lexical &cmp';

my &l = &infix:<leg>;
is ([[&l]] 10, 9), Less, '[[&l]] over leg is a plain left fold';
is ([[&l]] 1, 2, 3), More, '... also for more than two operands';

is ("%04x" X[&sprintf] 7, 11, 42), "0007 000b 002a", 'X[&name] takes a comma-list right operand';

sub foo($a, $b) { "$a-$b" }
is-deeply ((1, 2) Z[&foo] (3, 4)), ("1-3", "2-4"), 'Z[&foo] zips with a named sub';

sub z(&op, $a, $b) { (($a,) Z[&op] ($b,))[0] }
is z(&infix:<after>, 10, 9), True, 'Z[&op] with a &op parameter';
sub x(&op) { (1, 2) X[&op] (10, 20) }
is-deeply x(&infix:<+>), (11, 21, 12, 22), 'X[&op] with a &op parameter';
sub h(&op) { (1, 2) »[&op]« (3, 4) }
is-deeply h(&infix:<+>), (4, 6), '»[&op]« with a &op parameter';
sub r(&op) { [[&op]] 1, 2, 3 }
is r(&infix:<->), -4, '[[&op]] with a &op parameter';
sub b(&op, $a, $b) { $a [&op] $b }
is b(&infix:<->, 10, 9), 1, '[&op] with a &op parameter';
sub rb(&op, $a, $b) { $a R[&op] $b }
is rb(&infix:<->, 10, 9), -1, 'R[&op] with a &op parameter';
