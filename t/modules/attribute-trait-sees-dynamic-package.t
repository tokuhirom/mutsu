use Test;

# From AttrX::Mooish (and Async::Workers, Cooklang, OO::Plugin): an attribute
# trait handler reads the package being declared as `$*PACKAGE`, as a method
# trait handler does.

plan 2;

my @seen;

multi trait_mod:<is>(Attribute:D $attr, :$tagged!) {
    push @seen, $*PACKAGE.^name;
    push @seen, $*PACKAGE.HOW.^name;
}

my class Holder {
    has $.a is tagged;
}
is @seen[0], 'Holder', 'the class being declared is $*PACKAGE in a class trait';
is @seen[1], 'Perl6::Metamodel::ClassHOW', 'with its own HOW';
