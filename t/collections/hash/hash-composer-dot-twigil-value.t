use v6;
use Test;

plan 9;

# `{ a => $.g }` composes a HASH. The `$.`/`@.`/`%.`/`&.` twigil is an attribute
# accessor -- a term whose invocant is `self` -- not the invocant-less `.method`
# call that would make the braces a block. The hash-vs-block scan read the `.`
# in `$.g` as an implicit-topic call, so every `{ key => $.attr }` in a class
# came out a Block (or, with a control keyword for a key, failed to parse at
# all: "Preceding context expects a term, but found infix =>"). That is the
# shape Data::Dump::Tree's `DDTR::FixedGlyphs` uses.
# Every assertion below was checked against rakudo itself.

class Glyphs {
    has $.scalar = 'x';
    has @.array  = (1, 2);
    has %.hash   = (k => 'v');

    method scalar-value  { { a => $.scalar } }
    method array-value   { { a => @.array } }
    method hash-value    { { a => %.hash } }
    method keyword-key   { { last => $.scalar, empty => ' ' x $.scalar.chars } }
    method computed      { { a => $.scalar.uc } }
    method private-twin  { { a => $!scalar } }
}

my $g = Glyphs.new;

is-deeply $g.scalar-value, {a => 'x'}, '{ a => $.attr } composes a hash';
is $g.array-value.WHAT.^name, 'Hash', '{ a => @.attr } composes a hash';
is $g.hash-value.WHAT.^name, 'Hash', '{ a => %.attr } composes a hash';
is-deeply $g.computed, {a => 'X'}, 'a method call on the attribute still composes a hash';
is-deeply $g.private-twin, {a => 'x'}, 'the private `$!attr` twin is unchanged';
is-deeply $g.keyword-key, {last => 'x', empty => ' '},
    'a control-flow keyword as a key composes a hash alongside an attribute value';

# The rule must stay narrow: a genuinely invocant-less method call is still a
# topic reference, and so still forces a block.
is { a => .uc }.WHAT.^name, 'Block', 'a bare `.method` value is still a block';
is { a => 2 % .elems }.WHAT.^name, 'Block',
    'an infix `%` before a topic call is not the `%.` twigil';
is { a => .[0] }.WHAT.^name, 'Block', 'a bare `.[0]` value is still a block';

done-testing;
