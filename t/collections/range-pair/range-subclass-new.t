use Test;

# A user subclass of `Range` inherits `Range.new($min, $max, :excludes-min,
# :excludes-max)` and `Range`'s methods, renders as its range and accepts
# through `Range.ACCEPTS` (#14015). A built-in payload subclass of `Int` or
# `Str` accepts through its payload too.

plan 16;

class Q is Range {}
my $q = Q.new(1, 5);
isa-ok $q, Q, 'the constructor builds the subclass';
isa-ok $q, Range, 'which is a Range';
is $q.min, 1, 'min';
is $q.max, 5, 'max';
is $q.elems, 5, 'elems';
is $q.list, (1, 2, 3, 4, 5), 'list';
is $q.sum, 15, 'sum';
is Q.new(1, 5, :excludes-max).list, (1, 2, 3, 4), ':excludes-max';
is $q.Str, '1 2 3 4 5', 'Str is the range Str';
is $q.raku, '1..5', 'raku renders the range';
ok 3 ~~ $q, 'smartmatch accepts an element';
nok 7 ~~ $q, 'and refuses one outside';

class I is Int {}
ok 3 ~~ I.new(3), 'an Int subclass accepts its value';
nok 4 ~~ I.new(3), 'and refuses another';
class S is Str {}
ok 'a' ~~ S.new(value => 'a'), 'a Str subclass accepts its value';
nok 'b' ~~ S.new(value => 'a'), 'and refuses another';
