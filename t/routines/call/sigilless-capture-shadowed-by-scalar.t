use v6;
use Test;

plan 3;

# Found via the Intl::CLDR ecosystem distribution: a closure reading an
# enclosing sigilless binding (`\attr`) read its OWN same-named `$attr` local
# (Any) once it declared `my $attr = attr`.
sub escaping(\attr) { anon sub { my $attr = attr; $attr } }
is escaping(8)(), 8, 'my $attr = attr inside an escaping closure reads the outer \attr';

sub bound(\attr) { anon sub { my $attr := attr; $attr } }
is bound(9)(), 9, 'my $attr := attr reads the outer \attr';

multi sub trait_mod:<is>(Attribute \attr, :$lazy5!) {
    attr.package.^add_method: 'foo5', anon method {
        my $attr := attr;
        $attr.name;
    }
}
class C { has $!x is lazy5 }
is C.new.foo5, '$!x', 'the same shape inside a trait_mod anon method';
