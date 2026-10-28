use v6;
use Test;

plan 2;

# A candidate that is nearer the argument only by an unrelated first parameter
# (a role mixed into the argument instance) is not narrower than one whose
# second and third parameters are narrower by declared type, so it must not
# prune it (#15085, XML::Class's `deserialise`). rakudo tries the candidates of
# a tier in declaration order, and the earlier `where` one binds.
role ElementWrapper {}
role DeserialiseX {}
class Node {}
class Element is Node does Positional {}
class Text is Node {}
class Foo { has $.x }
subset Short of Int where * < 100;
my subset SoA of Attribute where { $_ !~~ DeserialiseX };
my subset TypedNode of Node where * ~~ Text|ElementWrapper;

multi sub deserialise(TypedNode $element, DeserialiseX $attribute, $obj, Str :$namespace) { "custom" }
multi sub deserialise(TypedNode $element, SoA $attribute, Bool $obj, Str :$namespace) { "bool" }
multi sub deserialise(TypedNode $element, SoA $attribute, $obj where { $_.HOW ~~ Metamodel::SubsetHOW }, Str :$namespace) { "subset" }
multi sub deserialise(TypedNode $element, SoA $attribute, Real $obj where { $_.HOW !~~ Metamodel::SubsetHOW }, Str :$namespace) { "real" }
multi sub deserialise(ElementWrapper $element is copy, Attribute $attribute, Mu $obj, Str :$namespace, Bool :$outer) { "Mu" }

my $e = Element.new;
$e does ElementWrapper;
my $attr = Foo.^attributes[0];

is deserialise($e, $attr, Short, :namespace("n")), 'subset',
    'the narrower-by-declared-types where candidate is not pruned by the nearer unrelated one';
is deserialise($e, $attr, Bool, :namespace("n")), 'bool', 'a typed candidate still wins where it binds';
