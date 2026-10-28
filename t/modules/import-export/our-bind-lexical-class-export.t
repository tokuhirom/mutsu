use v6;
use Test;
use lib 't/lib-our-bind-lexical-class';
use OurBindLexicalClass;

plan 3;

# A `my class` bound into the module's package with `OUR::<Foo> := Foo` and
# exported through `OUR::<DEFAULT> := Module` arrives as the class, not its name.
is Foo.^name, 'Foo', 'the imported symbol is the class';
isa-ok Foo, Mu, 'it is a type object, not a Str';
is Foo.hi, 'hi', 'and its methods are callable';
