use v6;
use Test;

# A bare (unparenthesized) trailing comma on a single `return` value is a
# separator, not a 1-element list constructor: `return 5,` yields the scalar `5`
# (Rakudo). The parenthesized `(5,)` form still builds a 1-element List, and a
# multi-element trailing comma (`return 5, 6,`) still builds a List. List
# contexts (array assignment, block value) are unaffected.

plan 9;

sub ret-int(--> Int) { return 5, }
is ret-int(), 5, 'return EXPR, yields the scalar (typed --> Int lives)';

# The real-world shape from zef's `Zef::Distribution!long-name`: a listop
# `sprintf` with a trailing comma inside a `--> Str` routine.
sub long-name($name --> Str) {
    return sprintf '%s:ver<%s>:auth<%s>:api<%s>',
        $name,
        ('0.0.1'),
        (''),
        (''),
    ;
}
is long-name('Foo::Bar'), 'Foo::Bar:ver<0.0.1>:auth<>:api<>',
    'multiline sprintf listop with trailing comma returns a Str';

# Parenthesized single element is still a 1-element List even in return position.
sub ret-parens { return (5,) }
isa-ok ret-parens(), List, 'return (5,) is a List';
is ret-parens().elems, 1, 'return (5,) has one element';

# Multi-element trailing comma keeps every element.
sub ret-two { return 5, 6, }
is ret-two().elems, 2, 'return EXPR, EXPR, keeps both elements';

# --- list contexts are unaffected by the return-only change ---

# Array assignment: a trailing comma keeps an Iterable unflattened.
my @a = 1..5,;
is @a.raku, '[1..5,]', 'my @a = 1..5, keeps the Range as a single element';

my @b = 5,;
is @b.raku, '[5]', 'my @b = 5, is a one-element array';

# A bare block value with a trailing comma is still a 1-element List.
sub blk { 5, }
isa-ok blk(), List, 'bare block value { 5, } is a List';
is blk().elems, 1, 'bare block value { 5, } has one element';
