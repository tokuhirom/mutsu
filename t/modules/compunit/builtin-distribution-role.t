use v6;
use Test;

# `Distribution` is a built-in Raku interface role (required stub methods `meta`
# and `content`). User distribution classes compose it and supply the
# implementations -- e.g. `Zef::Distribution does Distribution`. mutsu previously
# rejected `does Distribution` with X::InvalidType because the role was unknown.

plan 5;

class MyDist does Distribution {
    has %.metadata;
    method meta { %!metadata }
    method content($address) { "content-of-$address" }
}

my $d = MyDist.new(metadata => { name => 'Foo', ver => '1.0' });

ok $d ~~ Distribution, 'an instance doing Distribution smartmatches the role';
ok MyDist ~~ Distribution, 'the class type-conforms to Distribution';
is $d.meta<name>, 'Foo', 'overridden meta method works';
is $d.content('lib/Foo.rakumod'), 'content-of-lib/Foo.rakumod', 'overridden content method works';

# A plain class that does NOT do the role is not a Distribution.
class NotADist { }
nok NotADist.new ~~ Distribution, 'an unrelated class does not smartmatch Distribution';
