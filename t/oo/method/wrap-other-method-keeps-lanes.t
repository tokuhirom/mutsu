use Test;

# A program that wraps one method keeps every other method's answer, and a
# wrap of the method being called still runs, whichever fast lane the call
# would otherwise take (`.^name`, `.HOW`, `.clone`, an Attribute metamethod,
# a no-argument method on a Str).

plan 8;

class Foo { has $.x; method bump { $!x + 1 } }
my $h = Foo.^find_method('bump').wrap(-> $self { callsame() * 10 });

my $f = Foo.new(:x(1));
is $f.bump, 20, 'the wrapped method runs its wrapper';
is $f.^name, 'Foo', '.^name with an unrelated wrap';
ok $f.HOW ~~ Metamodel::ClassHOW, '.HOW with an unrelated wrap';
is $f.clone(:x(5)).x, 5, '.clone with an unrelated wrap';
is Foo.^attributes[0].name, '$!x', 'Attribute.name with an unrelated wrap';
is "abc".chars, 3, 'Str.chars with an unrelated wrap';

Str.^find_method('chars').wrap(-> $self { callsame() + 100 });
is "abc".chars, 103, 'a wrapped Str.chars runs its wrapper';

$h.restore;
is $f.bump, 2, 'restoring the wrap restores the method';
