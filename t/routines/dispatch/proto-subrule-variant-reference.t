use v6;
use Test;

plan 2;

# A proto candidate reference can use the same nested angle-bracket spelling
# that ASN::Grammar uses in its parser rules.
grammar ProtoSubruleVariantReference {
    proto token value { * }
    token value:sym<number> { \d+ }
    token TOP { <value:sym<number>> }
}

my $match = ProtoSubruleVariantReference.parse('42');
ok $match.defined, 'nested proto candidate reference parses';
is $match.Str, '42', 'nested proto candidate reference consumes the input';
