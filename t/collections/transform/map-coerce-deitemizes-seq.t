use Test;

# `.Map` (and a `--> Map()` return coercion) stores its values bare: a Seq
# value is not `$`-held. Since a `$`-held Seq stays one element in an `@`
# container (#18193), an itemized Seq left in the Map made `C.new(|$map)` bind
# `@.a` to the Seq itself (RedFactory t/03-not-model.rakutest: "Type check
# failed for an element of @!blas2; expected Bla but got Seq").

plan 8;

class C { has Int @.a }
sub s() { (1, 2, 3).Seq }

is (b => s(),).Map.raku, 'Map.new((:b((1, 2, 3).Seq)))', '.Map of a list of Pairs holds the Seq bare';
my %m is Map = (b => s(),);
is %m.raku, 'Map.new((:b((1, 2, 3).Seq)))', 'my %m is Map = ... holds the Seq bare';

sub coerced(--> Map()) { (a => s(),) }
is coerced().raku, 'Map.new((:a((1, 2, 3).Seq)))', 'a --> Map() return holds the Seq bare';
is C.new(|coerced()).a.elems, 3, 'a Seq from a coerced Map fills an @ attribute element-wise';
is-deeply C.new(|(a => s(),).Map).a, Array[Int].new(1, 2, 3), 'C.new(|$list.Map) binds @.a to the values';

my %h = a => s();
is %h.raku, '{:a($((1, 2, 3).Seq))}', 'a Hash still itemizes its Seq value';
is %h.Map.raku, 'Map.new((:a((1, 2, 3).Seq)))', '.Map of that Hash decontainerizes it';

my $item = s();
my @a = $item;
is @a.elems, 1, 'a $-held Seq still stays one element in an @ assignment';
