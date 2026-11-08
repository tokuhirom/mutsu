use Test;
use experimental :rakuast;

plan 9;

# A grammar alternative's or a method's `:sym` adverb is a colonpair of the
# declaration's Name, as rakudo builds it, not part of one identifier string.

my $g = Q[grammar G { proto token alt {*}; token alt:sym<foo> { x }; token alt:sym«b ar» { y } }].AST;
my $alts = $g.find-nodes(RakuAST::TokenDeclaration);
my $foo = $alts.first({ .name.colonpairs.elems });
is $foo.name.canonicalize, 'alt:sym<foo>', 'the name canonicalizes to the flat spelling';
is $foo.name.colonpairs.elems, 1, 'one colonpair';
my $pair = $foo.name.colonpairs[0];
isa-ok $pair, RakuAST::ColonPair::Value, 'a ColonPair::Value';
is $pair.key, 'sym', 'its key is sym';
is $pair.value.processors.join(' '), 'words val', 'its value is a word quote';

my $q = $alts[2];
is $q.name.colonpairs[0].value.processors.join(' '), 'quotewords val', 'a «» value is a quotewords quote';

my $m = Q[class C { method m:sym<z>() { } }].AST.find-nodes(RakuAST::Method)[0];
is $m.name.colonpairs.elems, 1, 'a method name carries the colonpair too';

# The tree runs: a method named with a :sym adverb is found by its full name.
my $ast = Q[class P2 { method m:sym<z>() { 42 } }].AST;
EVAL $ast;
is ::('P2').new."m:sym<z>"(), 42, 'an EVAL of the tree keeps the :sym method name';
is Q[class P3 { method n:sym«a b»() { 7 } }].AST.find-nodes(RakuAST::Method)[0].name.colonpairs[0].value.processors[0], 'quotewords', 'a «» method adverb';
