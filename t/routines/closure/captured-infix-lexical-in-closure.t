use Test;

plan 8;

# A closure that names the enclosing routine's `&infix:<op>` lexical (a
# parameter) captures it: called after the routine returned, it still uses
# THAT binding, not the builtin operator (#16967).
sub explicit(&infix:<cmp>) { -> $x, $y { &infix:<cmp>($x, $y) } }
is explicit({ $^b <=> $^a })(1, 2).raku, 'Order::More', 'explicit call in a returned pointy block';

sub operator(&infix:<cmp>) { -> $x, $y { $x cmp $y } }
is operator({ $^b <=> $^a })(1, 2).raku, 'Order::More', 'operator syntax in a returned pointy block';

sub whatever(&infix:<cmp>) { * cmp * == More }
is whatever({ $^b <=> $^a })(1, 2), True, 'a returned WhateverCode';

class Holder { has &.test; method go($a, $b) { &!test($a, $b) } }
sub held(&infix:<cmp>) { Holder.new(test => { $^a cmp $^b == More }) }
is held({ $^b <=> $^a }).go(1, 2), True, 'a closure stored in an attribute and called later';

role Gen[&test] { method go($a, $b) { test($a, $b) } }
sub parametrized(&infix:<cmp>) { Gen[* cmp * == More].new }
is parametrized({ $^b <=> $^a }).go(1, 2), True, 'a closure used as a role parameter';

sub other(&infix:<cmp>) { -> $x, $y { &infix:<cmp>($x, $y) } }
is other({ $^a <=> $^b })(1, 2).raku, 'Order::Less', 'a second call with a different comparator';

# The builtin is untouched where no lexical is declared.
my &plain = -> $x, $y { $x cmp $y };
is plain(1, 2).raku, 'Order::Less', 'builtin cmp in a closure with no lexical';
sub ordinary(&c) { -> $x, $y { &c($x, $y) } }
is ordinary({ $^b <=> $^a })(1, 2).raku, 'Order::More', 'an ordinary &-parameter still captures';
