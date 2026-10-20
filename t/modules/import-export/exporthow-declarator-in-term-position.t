use Test;
use OO::Monitors;

# From the Red distribution (`(model Foo { })`, `given model C is table<c> { } { }`,
# `model :: is table<t> { }.^create-table`): a declarator exported through
# EXPORTHOW::DECLARE is a term wherever `class` is. Not runnable under a stock
# rakudo without OO::Monitors installed; expected values follow `class`.
plan 6;

is (monitor Foo { method m { 1 } }).^name, 'Foo', 'parenthesized named';
my $m = monitor Bar { method m { 2 } };
is $m.new.m, 2, 'initializer';
given monitor Baz { method z { 3 } } {
    is .new.z, 3, 'given topic';
}
is (monitor :: { method q { 4 } }).new.q, 4, 'anonymous with ::';
is (monitor { method q { 5 } }).new.q, 5, 'anonymous bare block';
monitor Stmt { method s { 6 } }
is Stmt.new.s, 6, 'statement form still works';
