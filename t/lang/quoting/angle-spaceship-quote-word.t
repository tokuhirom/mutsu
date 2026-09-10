use v6;
use Test;

# A tightly-bound angle bracket `<=>` (and `<=foo>`, `<==>`) in *term*,
# *subscript*, and *colonpair-value* position is an ordinary quote-word / key /
# value (`'='`, `'=foo'`, `'=='`), NOT the `<=>` spaceship / `<=` infix operator
# (those need surrounding whitespace). Regression: three separate angle parsers
# each rejected a leading `=`, so `<=>`, `%h<=>`, and `:name<=>` all failed to
# parse or bound the wrong value. (Hit by Bench's `:fill<=>` / `%h<=>` usage.)

plan 12;

# Term-position quote-word.
is <=>, '=',        'bare <=> quote-word is the string "="';
is <==>, '==',      'bare <==> quote-word is "=="';
is <=foo>, '=foo',  'bare <=foo> quote-word is "=foo"';

# Hash subscript with `=` key.
{
    my %h = ('=' => 9, '=foo' => 3);
    is %h<=>, 9,      'hash subscript %h<=> keys on "="';
    is %h<=foo>, 3,   'hash subscript %h<=foo> keys on "=foo"';
}

# Colonpair angle value.
{
    sub f(:$fill?) { $fill }
    is f(:fill<=>), '=',       'colonpair :fill<=> is fill => "="';
    is f(:fill<=foo>), '=foo', 'colonpair :fill<=foo> is fill => "=foo"';
}

# Colonpair in a hash literal.
{
    my %h = (:sep<=>);
    is %h<sep>, '=', 'hash-literal colonpair :sep<=> is sep => "="';
}

# The `<=>` spaceship and `<=` operators still work (whitespace-separated).
is (1 <=> 2), Order::Less,  'spaceship 1 <=> 2 is Less';
is (5 <=> 5), Order::Same,  'spaceship 5 <=> 5 is Same';
ok (3 <= 5),                'infix 3 <= 5 is True';
is ([<=] 1, 2, 3), True,    'reduction [<=] still works';
