use Test;
use experimental :rakuast;

# A second deparse of the same shapes reuses the memoised candidate tiers of
# rakudo's RakuAST::Deparse (267 multi candidates); the answer must not change,
# and a subclass overriding one candidate must still win after the base
# class's tiers are warm.

plan 4;

my $first  = Q|say 1 + 2;|.AST.DEPARSE;
my $second = Q|say 1 + 2;|.AST.DEPARSE;
is $second, $first, 'a repeated deparse answers the same';
is $first, "say 1 + 2\n", 'and it is what raku prints';

my class Loud is RakuAST::Deparse {
    multi method deparse(RakuAST::IntLiteral:D $ast --> Str:D) { 'ONE' }
}
is Q|say 1;|.AST.DEPARSE(Loud), "say ONE\n", 'a subclass candidate wins over the warm base tiers';
is Q|say 1;|.AST.DEPARSE, "say 1\n", 'the base deparser is untouched';
